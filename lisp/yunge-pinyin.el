;;; yunge-pinyin.el --- Bounded Pinyin search compiler -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This Pinyin query compiler is shared by Evil search, Orderless, Avy, and
;; other regexp consumers.
;;
;; A query is segmented with a bounded syllable DAG.  Legal full syllables,
;; incomplete final syllables, and initial abbreviations are supported.  The
;; best segmentations are compiled into cached character classes.  A semantic
;; complexity budget prevents broad initial-only queries from producing huge,
;; slow regexps; the literal query is always retained as a safe fallback.

;;; Code:

(require 'cl-lib)
(require 'regexp-opt)
(require 'seq)
(require 'subr-x)

(defgroup yunge-pinyin nil
  "Pinyin-aware search support."
  :group 'matching)

(defcustom yunge-pinyin-regexp-budget 3200
  "Maximum size and estimated matching cost of an expanded regexp.
The literal query is returned even when it alone exceeds this value."
  :type 'integer)

(defcustom yunge-pinyin-max-frequency-level 4
  "Highest character-frequency level used automatically.
Levels 1 through 4 cover progressively less common modern-use characters.
Level 5 additionally includes the rare and historical remainder of Unihan."
  :type 'integer)

(defcustom yunge-pinyin-regexp-cache-size 256
  "Maximum number of complete query regexps retained in memory.
Set this to zero to disable the complete-query cache."
  :type 'integer)

(defcustom yunge-pinyin-max-segmentations 4
  "Maximum number of ranked Pinyin segmentations compiled for one run."
  :type 'integer)

(defcustom yunge-pinyin-segmentation-cost-slack 12
  "Maximum cost above the best segmentation retained as an alternative."
  :type 'integer)

(defcustom yunge-pinyin-initial-penalty 96
  "Complexity charged for each abbreviated initial in a segmentation.
This models the high false-positive cost of broad initial character classes."
  :type 'integer)

(defconst yunge-pinyin--letter-characters
  (concat "A-Za-z"
          "üÜāáǎàĀÁǍÀēéěèĒÉĚÈêÊīíǐìĪÍǏÌ"
          "ōóǒòŌÓǑÒūúǔùŪÚǓÙǖǘǚǜǕǗǙǛ"
          "ḿḾńňǹŃŇǸ"))

(defconst yunge-pinyin--letter-run-regexp
  (format "[%s][%s']*"
          yunge-pinyin--letter-characters
          yunge-pinyin--letter-characters))

(defconst yunge-pinyin--abbreviation-prefixes
  '("b" "p" "m" "f" "d" "t" "n" "l" "g" "k" "h"
    "j" "q" "x" "zh" "ch" "sh" "r" "z" "c" "s" "y" "w"
    "a" "e" "o"))

(defconst yunge-pinyin--tone-replacements
  '(("ā" . "a") ("á" . "a") ("ǎ" . "a") ("à" . "a")
    ("ē" . "e") ("é" . "e") ("ě" . "e") ("è" . "e")
    ("ê" . "e")
    ("ī" . "i") ("í" . "i") ("ǐ" . "i") ("ì" . "i")
    ("ō" . "o") ("ó" . "o") ("ǒ" . "o") ("ò" . "o")
    ("ū" . "u") ("ú" . "u") ("ǔ" . "u") ("ù" . "u")
    ("ǖ" . "v") ("ǘ" . "v") ("ǚ" . "v") ("ǜ" . "v")
    ("ü" . "v")
    ("ḿ" . "m") ("ń" . "n") ("ň" . "n") ("ǹ" . "n")))

(cl-defstruct (yunge-pinyin--token
               (:constructor yunge-pinyin--make-token))
  text
  prefixp)

(cl-defstruct (yunge-pinyin--path
               (:constructor yunge-pinyin--make-path))
  tokens
  cost
  (cached-signature 'uncomputed)
  (cached-order-key 'uncomputed))

(defvar yunge-pinyin--syllable-table nil)
(defvar yunge-pinyin--prefix-table nil)
(defvar yunge-pinyin--maximum-syllable-length 0)
(defvar yunge-pinyin--class-cache (make-hash-table :test #'equal))
(defvar yunge-pinyin--character-count-cache
  (make-hash-table :test #'equal))
(defvar yunge-pinyin--regexp-cache (make-hash-table :test #'equal))
(defvar yunge-pinyin--regexp-cache-order nil)
(defvar yunge-pinyin--regexp-cache-tail nil)

(defvar yunge-pinyin-data)

(defun yunge-pinyin-normalize (text)
  "Return lowercase, toneless Pinyin for TEXT, using v for ü."
  (let ((result (downcase text)))
    (setq result (string-replace "u:" "v" result))
    (dolist (replacement yunge-pinyin--tone-replacements)
      (setq result
            (string-replace (car replacement) (cdr replacement) result)))
    result))

(defun yunge-pinyin--initialize ()
  "Initialize syllable and prefix indexes on first use."
  (unless yunge-pinyin--syllable-table
    (require 'yunge-pinyin-data)
    (setq yunge-pinyin--syllable-table (make-hash-table :test #'equal)
          yunge-pinyin--prefix-table (make-hash-table :test #'equal)
          yunge-pinyin--maximum-syllable-length 0)
    (dolist (entry yunge-pinyin-data)
      (let ((syllable (car entry)))
        (puthash syllable (cdr entry) yunge-pinyin--syllable-table)
        (setq yunge-pinyin--maximum-syllable-length
              (max yunge-pinyin--maximum-syllable-length
                   (length syllable)))
        (dotimes (index (length syllable))
          (let ((prefix (substring syllable 0 (1+ index))))
            (puthash prefix
                     (cons syllable (gethash prefix yunge-pinyin--prefix-table))
                     yunge-pinyin--prefix-table)))))))

(defun yunge-pinyin-clear-cache ()
  "Clear compiled Pinyin character-class and complete-query caches."
  (interactive)
  (clrhash yunge-pinyin--class-cache)
  (clrhash yunge-pinyin--character-count-cache)
  (clrhash yunge-pinyin--regexp-cache)
  (setq yunge-pinyin--regexp-cache-order nil
        yunge-pinyin--regexp-cache-tail nil))

(defun yunge-pinyin--path-signature (path)
  "Return a stable deduplication signature for PATH."
  (let ((signature (yunge-pinyin--path-cached-signature path)))
    (if (eq signature 'uncomputed)
        (setf (yunge-pinyin--path-cached-signature path)
              (mapcar (lambda (token)
                        (cons (yunge-pinyin--token-prefixp token)
                              (yunge-pinyin--token-text token)))
                      (yunge-pinyin--path-tokens path)))
      signature)))

(defun yunge-pinyin--path-order-key (path)
  "Return a cached lexical ordering key for PATH."
  (let ((key (yunge-pinyin--path-cached-order-key path)))
    (if (eq key 'uncomputed)
        (setf (yunge-pinyin--path-cached-order-key path)
              (prin1-to-string (yunge-pinyin--path-signature path)))
      key)))

(defun yunge-pinyin--path-less-p (left right)
  "Return non-nil when LEFT ranks before RIGHT."
  (let ((left-cost (yunge-pinyin--path-cost left))
        (right-cost (yunge-pinyin--path-cost right)))
    (if (/= left-cost right-cost)
        (< left-cost right-cost)
      (string< (yunge-pinyin--path-order-key left)
               (yunge-pinyin--path-order-key right)))))

(defun yunge-pinyin--prune-paths (paths limit)
  "Sort, deduplicate, and retain at most LIMIT members of PATHS."
  (let ((seen (make-hash-table :test #'equal))
        best-cost
        result)
    (dolist (path (sort paths #'yunge-pinyin--path-less-p))
      (unless best-cost
        (setq best-cost (yunge-pinyin--path-cost path)))
      (when (<= (yunge-pinyin--path-cost path)
                (+ best-cost yunge-pinyin-segmentation-cost-slack))
        (let ((signature (yunge-pinyin--path-signature path)))
          (unless (gethash signature seen)
            (puthash signature t seen)
            (push path result)))))
    (seq-take (nreverse result) limit)))

(defun yunge-pinyin--edge-cost (text prefixp terminalp)
  "Return the segmentation cost of TEXT.
PREFIXP identifies an abbreviation or incomplete syllable.  TERMINALP means
the token reaches the end of the input."
  (cond
   ((and prefixp (= (length text) 1))
    (+ 20 yunge-pinyin-initial-penalty))
   ((and prefixp (member text '("zh" "ch" "sh")))
    (* 2 (+ 20 yunge-pinyin-initial-penalty)))
   (prefixp
    (+ 10 (- yunge-pinyin--maximum-syllable-length
             (min (length text) yunge-pinyin--maximum-syllable-length))))
   (t
    (+ (if terminalp 2 0)
       (- yunge-pinyin--maximum-syllable-length
          (min (length text) yunge-pinyin--maximum-syllable-length))))))

(defun yunge-pinyin--edges-at (text beginning allow-incomplete)
  "Return legal segmentation edges in TEXT starting at BEGINNING.
ALLOW-INCOMPLETE permits the final edge to be a syllable prefix."
  (let ((end (length text))
        edges)
    (cl-loop
     for token-end from (1+ beginning)
     to (min end (+ beginning yunge-pinyin--maximum-syllable-length))
     for candidate = (substring text beginning token-end)
     for terminalp = (= token-end end)
     do
     (cond
      ((and terminalp allow-incomplete
            (gethash candidate yunge-pinyin--prefix-table))
       (push (list token-end
                   (yunge-pinyin--make-token
                    :text candidate :prefixp t)
                   (yunge-pinyin--edge-cost candidate t t))
             edges))
      ((and (gethash candidate yunge-pinyin--syllable-table)
            ;; A consonant written alone inside a run is overwhelmingly an
            ;; abbreviation, not an interjection syllable such as m or n.
            (or terminalp
                (> (length candidate) 1)
                (member candidate '("a" "e" "o"))))
       (push (list token-end
                   (yunge-pinyin--make-token
                    :text candidate :prefixp nil)
                   (yunge-pinyin--edge-cost candidate nil nil))
             edges)))
     (when (and (not terminalp)
                (member candidate yunge-pinyin--abbreviation-prefixes))
       (push (list token-end
                   (yunge-pinyin--make-token
                    :text candidate :prefixp t)
                   (yunge-pinyin--edge-cost candidate t nil))
             edges)))
    edges))

(defun yunge-pinyin--segment-one (text &optional exact-end)
  "Return ranked segmentations for apostrophe-free Pinyin TEXT.
When EXACT-END is non-nil, require the final token to be a complete syllable."
  (yunge-pinyin--initialize)
  (let* ((length (length text))
         (candidate-limit (max 12 (* yunge-pinyin-max-segmentations 4)))
         (best (make-vector (1+ length) nil)))
    (aset best length
          (list (yunge-pinyin--make-path :tokens nil :cost 0)))
    (cl-loop
     for beginning downfrom (1- length) to 0
     do
     (let (candidates)
       (dolist (edge (yunge-pinyin--edges-at text beginning (not exact-end)))
         (pcase-let ((`(,end ,token ,edge-cost) edge))
           (dolist (tail (aref best end))
             (push
              (yunge-pinyin--make-path
               :tokens (cons token (yunge-pinyin--path-tokens tail))
               :cost (+ edge-cost (yunge-pinyin--path-cost tail)))
              candidates))))
       (aset best beginning
             (yunge-pinyin--prune-paths candidates candidate-limit))))
    (seq-take (aref best 0) yunge-pinyin-max-segmentations)))

(defun yunge-pinyin--combine-segmentations (left right limit)
  "Combine LEFT and RIGHT path lists, retaining at most LIMIT paths."
  (let (result)
    (dolist (left-path left)
      (dolist (right-path right)
        (push
         (yunge-pinyin--make-path
          :tokens (append (yunge-pinyin--path-tokens left-path)
                          (yunge-pinyin--path-tokens right-path))
          :cost (+ (yunge-pinyin--path-cost left-path)
                   (yunge-pinyin--path-cost right-path)))
         result)))
    (yunge-pinyin--prune-paths result limit)))

(defun yunge-pinyin--segment-run (run)
  "Return internal ranked segmentations for Pinyin RUN."
  (let ((parts (split-string (yunge-pinyin-normalize run) "'" t))
        (paths (list (yunge-pinyin--make-path :tokens nil :cost 0))))
    (while (and parts paths)
      (let ((part-paths
             (yunge-pinyin--segment-one (pop parts) (not (null parts)))))
        (setq paths
              (and part-paths
                   (yunge-pinyin--combine-segmentations
                    paths part-paths
                    (max 12 (* yunge-pinyin-max-segmentations 4)))))))
    (seq-take paths yunge-pinyin-max-segmentations)))

(defun yunge-pinyin-segmentations (text)
  "Return ranked Pinyin syllable segmentations for ASCII TEXT.
Each result is a list of strings.  Apostrophes force syllable boundaries."
  (delete-dups
   (mapcar
    (lambda (path)
      (mapcar #'yunge-pinyin--token-text
              (yunge-pinyin--path-tokens path)))
    (yunge-pinyin--segment-run text))))

(defun yunge-pinyin--token-syllables (token)
  "Return source syllables whose characters may satisfy TOKEN."
  (if (yunge-pinyin--token-prefixp token)
      (gethash (yunge-pinyin--token-text token)
               yunge-pinyin--prefix-table)
    (list (yunge-pinyin--token-text token))))

(defun yunge-pinyin--token-characters (token level)
  "Return characters satisfying TOKEN through frequency LEVEL."
  (let (strings)
    (dolist (syllable (yunge-pinyin--token-syllables token))
      (when-let* ((buckets (gethash syllable yunge-pinyin--syllable-table)))
        (dotimes (index level)
          (push (aref buckets index) strings))))
    (apply #'concat strings)))

(defun yunge-pinyin--token-cache-key (token level)
  "Return a cache key for TOKEN at LEVEL."
  (list (yunge-pinyin--token-prefixp token)
        (yunge-pinyin--token-text token)
        level))

(defun yunge-pinyin--token-base-cache-key (token)
  "Return a frequency-independent cache key for TOKEN."
  (cons (yunge-pinyin--token-prefixp token)
        (yunge-pinyin--token-text token)))

(defun yunge-pinyin--token-character-counts (token)
  "Return cumulative character counts for every frequency level of TOKEN."
  (let* ((key (yunge-pinyin--token-base-cache-key token))
         (cached (gethash key yunge-pinyin--character-count-cache 'missing)))
    (if (not (eq cached 'missing))
        cached
      (let ((bucket-counts (make-vector 5 0))
            (counts (make-vector 6 0)))
        (dolist (syllable (yunge-pinyin--token-syllables token))
          (when-let* ((buckets
                       (gethash syllable yunge-pinyin--syllable-table)))
            (dotimes (index (length buckets))
              (cl-incf (aref bucket-counts index)
                       (length (aref buckets index))))))
        (dotimes (index (length bucket-counts))
          (aset counts (1+ index)
                (+ (aref counts index) (aref bucket-counts index))))
        (puthash key counts yunge-pinyin--character-count-cache)
        counts))))

(defun yunge-pinyin--token-character-count (token level)
  "Return the uncompressed character count for TOKEN at LEVEL."
  (aref (yunge-pinyin--token-character-counts token) level))

(defun yunge-pinyin--token-regexp (token level)
  "Return a compact character regexp for TOKEN at LEVEL."
  (let* ((key (yunge-pinyin--token-cache-key token level))
         (cached (gethash key yunge-pinyin--class-cache 'missing)))
    (if (not (eq cached 'missing))
        cached
      (let* ((characters (yunge-pinyin--token-characters token level))
             (regexp
              (and (not (string-empty-p characters))
                   (regexp-opt-charset (string-to-list characters)))))
        (puthash key regexp yunge-pinyin--class-cache)
        regexp))))

(defun yunge-pinyin--path-character-stats (path level)
  "Return (TOTAL . MAXIMUM) character counts for PATH at LEVEL."
  (let ((total 0)
        (maximum 0))
    (dolist (token (yunge-pinyin--path-tokens path))
      (let ((count (yunge-pinyin--token-character-count token level)))
        (cl-incf total count)
        (setq maximum (max maximum count))))
    (cons total maximum)))

(defun yunge-pinyin--path-populated-p (path level)
  "Return non-nil when every token in PATH has characters at LEVEL."
  (cl-every
   (lambda (token)
     (> (yunge-pinyin--token-character-count token level) 0))
   (yunge-pinyin--path-tokens path)))

(defun yunge-pinyin--minimum-populated-level (paths maximum-level)
  "Return the first usable frequency level for any member of PATHS."
  (cl-loop
   for level from 1 to maximum-level
   when (cl-some (lambda (path)
                   (yunge-pinyin--path-populated-p path level))
                 paths)
   return level))

(defun yunge-pinyin--repeat-character-regexp (regexp count)
  "Return REGEXP repeated exactly COUNT times."
  (if (= count 1)
      regexp
    (format "\\(?:%s\\)\\{%d\\}" regexp count)))

(defun yunge-pinyin--compile-path (path level)
  "Compile PATH at frequency LEVEL, or return nil."
  (let (regexps parts valid)
    (setq valid t)
    (dolist (token (yunge-pinyin--path-tokens path))
      (if-let* ((regexp (yunge-pinyin--token-regexp token level)))
          (push regexp regexps)
        (setq valid nil)))
    (when valid
      (setq regexps (nreverse regexps))
      (while regexps
        (let ((regexp (pop regexps))
              (count 1))
          (while (and regexps (equal (car regexps) regexp))
            (pop regexps)
            (cl-incf count))
          (push (yunge-pinyin--repeat-character-regexp regexp count)
                parts)))
      (apply #'concat (nreverse parts)))))

(defun yunge-pinyin--alternation (regexps)
  "Return a noncapturing alternation of REGEXPS."
  (if (= (length regexps) 1)
      (car regexps)
    (concat "\\(?:" (string-join regexps "\\|") "\\)")))

(defun yunge-pinyin--compile-run-at-level (literal paths level limit)
  "Compile LITERAL and PATHS at LEVEL without exceeding LIMIT."
  (let ((remaining (- limit (length literal) 7))
        expansions)
    (dolist (path paths)
      (let* ((stats (yunge-pinyin--path-character-stats path level))
             (total (car stats))
             (maximum (cdr stats))
             (regexp
              (and (> remaining 0)
                   ;; Do not build a single character class that cannot fit
                   ;; by itself.  regexp-opt-charset on such rare tails is a
                   ;; large cold-cache cost whose result is then discarded.
                   (<= maximum remaining)
                   (<= (+ (yunge-pinyin--path-cost path) total)
                       (* 2 remaining))
                   (yunge-pinyin--compile-path path level))))
        (if (and regexp (<= (+ (length regexp) 2) remaining))
            (progn
              (push regexp expansions)
              (setq remaining (- remaining (length regexp) 2)))
          nil)))
    (when expansions
      (yunge-pinyin--alternation
       (cons literal (delete-dups (nreverse expansions)))))))

(defun yunge-pinyin--compile-run (run limit)
  "Compile Pinyin RUN into a regexp no longer than LIMIT."
  (let ((literal (regexp-quote run))
        (paths (yunge-pinyin--segment-run run))
        result)
    (when paths
      ;; Start at the first level where a complete path has characters.  If
      ;; that smallest semantic expansion cannot fit, every broader level is
      ;; more expensive and the run should stay literal.
      (let* ((maximum-level
              (max 1 (min 5 yunge-pinyin-max-frequency-level)))
             (minimum-level
              (yunge-pinyin--minimum-populated-level paths maximum-level))
             (minimum
              (and minimum-level
                   (yunge-pinyin--compile-run-at-level
                    literal paths minimum-level limit))))
        (when minimum
          (setq result
                (or
                 (cl-loop
                  for level downfrom maximum-level above minimum-level
                  for candidate =
                  (yunge-pinyin--compile-run-at-level
                   literal paths level limit)
                  when candidate return candidate)
                 minimum)))))
    (or result literal)))

(defun yunge-pinyin--valid-regexp-p (regexp)
  "Return non-nil when Emacs can compile REGEXP."
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun yunge-pinyin--regexp-cache-key (text)
  "Return a complete-query cache key for TEXT and compiler settings."
  (list text
        yunge-pinyin-regexp-budget
        yunge-pinyin-max-frequency-level
        yunge-pinyin-max-segmentations
        yunge-pinyin-segmentation-cost-slack
        yunge-pinyin-initial-penalty))

(defun yunge-pinyin--cache-regexp (key regexp)
  "Store REGEXP under KEY in the bounded complete-query cache."
  (let ((limit (max 0 yunge-pinyin-regexp-cache-size)))
    (when (> limit 0)
      (while (and yunge-pinyin--regexp-cache-order
                  (>= (hash-table-count yunge-pinyin--regexp-cache) limit))
        (let ((oldest (pop yunge-pinyin--regexp-cache-order)))
          (unless yunge-pinyin--regexp-cache-order
            (setq yunge-pinyin--regexp-cache-tail nil))
          (remhash oldest yunge-pinyin--regexp-cache)))
      (puthash key regexp yunge-pinyin--regexp-cache)
      (let ((cell (list key)))
        (if yunge-pinyin--regexp-cache-tail
            (setcdr yunge-pinyin--regexp-cache-tail cell)
          (setq yunge-pinyin--regexp-cache-order cell))
        (setq yunge-pinyin--regexp-cache-tail cell))))
  regexp)

(defun yunge-pinyin--regexp-uncached (text)
  "Return a bounded literal-or-Pinyin regexp for nonempty TEXT."
  (let* ((literal-regexp (regexp-quote text))
         (remaining-extra
          (max 0 (- yunge-pinyin-regexp-budget
                    (length literal-regexp))))
         (position 0)
         (expandedp nil)
         parts)
    (while (string-match yunge-pinyin--letter-run-regexp text position)
      (let* ((beginning (match-beginning 0))
             (end (match-end 0))
             (run (substring text beginning end))
             (literal-run (regexp-quote run))
             (compiled
              (yunge-pinyin--compile-run
               run (+ (length literal-run) remaining-extra)))
             (extra (max 0 (- (length compiled) (length literal-run)))))
        (push (regexp-quote (substring text position beginning)) parts)
        (push compiled parts)
        (setq expandedp (or expandedp (> extra 0))
              remaining-extra (- remaining-extra extra)
              position end)))
    (push (regexp-quote (substring text position)) parts)
    (let ((regexp (apply #'concat (nreverse parts))))
      (if (and expandedp (yunge-pinyin--valid-regexp-p regexp))
          regexp
        literal-regexp))))

(defun yunge-pinyin-regexp (text)
  "Return a bounded regexp matching TEXT literally or through Pinyin.
Pinyin letter runs are independently expanded.  Other characters are quoted.
When expansion would be too broad, only the affected literal run is kept."
  (unless (string-empty-p text)
    (let* ((plain-text (substring-no-properties text))
           (key (yunge-pinyin--regexp-cache-key plain-text))
           (cached (and (> yunge-pinyin-regexp-cache-size 0)
                        (gethash key yunge-pinyin--regexp-cache 'missing))))
      (if (and cached (not (eq cached 'missing)))
          cached
        (yunge-pinyin--cache-regexp
         key (yunge-pinyin--regexp-uncached plain-text))))))

(provide 'yunge-pinyin)

;;; yunge-pinyin.el ends here
