;;; shuying-org-source-test.el --- Org formula source tests -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'yunge-test-helper)
(require 'shuying-org-source)

(ert-deftest shuying-org-source-classifies-only-block-math-for-centering ()
  (with-temp-buffer
    (org-mode)
    (insert
     "$a$\n"
     "\\(b\\)\n"
     "$$c$$\n"
     "\\[d\\]\n"
     "\\begin{equation}\ne = f\n\\end{equation}\n")
    (should
     (equal
      (mapcar #'shuying-org-formula-block-math-p
              (shuying-org-source-formulas))
      '(nil nil t t t)))))

(ert-deftest shuying-org-source-leaves-whitespace-only-math-as-source ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\(\\)\n"
     "\\(  \\)\n"
     "\\[  \\]\n"
     "$$  $$\n"
     "\\begin{equation}\n"
     "  \n"
     "\\end{equation}\n"
     "\\(x\\)\n"
     "\\[y\\]\n"
     "\\begin{equation}\n"
     "z\n"
     "\\end{equation}\n")
    (let ((fragments (shuying-org-source-formulas)))
      (should
       (equal
        (mapcar #'shuying-org-formula-source fragments)
        '("\\(x\\)"
          "\\[y\\]"
          "\\begin{equation}\nz\n\\end{equation}\n")))
      (should
       (= (shuying-org-formula-equation-number (car (last fragments)))
          2)))))

(ert-deftest shuying-org-source-finds-only-explicit-latex-math ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\mathrm{A}\n"
     "$x$\n"
     "\\(y\\)\n"
     "\\[z\\]\n"
     "\\begin{equation}\nw = 1\n\\end{equation}\n")
    (goto-char (point-min))
    (should-not (shuying-org-source-at-position (point)))
    (should
     (equal
      (mapcar #'shuying-org-formula-source
              (shuying-org-source-formulas))
      '("$x$" "\\(y\\)" "\\[z\\]"
        "\\begin{equation}\nw = 1\n\\end{equation}\n")))))

(ert-deftest shuying-org-source-tracks-equation-numbering-context ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{equation}\na = b\n\\end{equation}\n\n"
     "$x$\n\n"
     "\\begin{align}\n"
     "a &= b \\\\\n"
     "c &= \\begin{aligned}\n"
     "  x & = y \\\\\n"
     "  z & = w\n"
     "\\end{aligned} \\nonumber \\\\\n"
     "d &= e \\tag{manual} \\\\\n"
     "% A commented \\\\ does not start another row.\n"
     "f &= g\n"
     "\\end{align}\n\n"
     "\\begin{equation}\nj = k \\tag{manual}\n\\end{equation}\n\n"
     "\\begin{multline}\np + q \\\\\n"
     "+ r = s\n\\end{multline}\n\n"
     "\\begin{equation}\nh = i\n\\end{equation}\n")
    (should
     (equal
      (mapcar #'shuying-org-formula-equation-number
              (shuying-org-source-formulas))
      '(1 nil 2 4 4 5)))))

(ert-deftest shuying-org-source-does-not-number-starred-environments ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{align*}\na &= b\n\\end{align*}\n\n"
     "\\begin{displaymath}\nx = y\n\\end{displaymath}\n\n"
     "\\begin{equation}\nc = d\n\\end{equation}\n")
    (should
     (equal
      (mapcar #'shuying-org-formula-equation-number
              (shuying-org-source-formulas))
      '(nil nil 1)))))

(ert-deftest shuying-org-source-renumbers-after-an-earlier-environment-changes ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{equation}\nx = y\n\\end{equation}\n\n"
     "\\begin{equation}\ny = z\n\\end{equation}\n")
    (should
     (equal
      (mapcar #'shuying-org-formula-equation-number
              (shuying-org-source-formulas))
      '(1 2)))
    (goto-char (point-min))
    (search-forward "x = y")
    (insert " \\tag{manual}")
    (should
     (equal
      (mapcar #'shuying-org-formula-equation-number
              (shuying-org-source-formulas))
      '(1 1)))))

(ert-deftest shuying-org-source-selects-formulas-from-disjoint-ranges ()
  (with-temp-buffer
    (org-mode)
    (insert "$a$ gap $b$ gap $c$")
    (let* ((fragments (shuying-org-source-formulas))
           (first (nth 0 fragments))
           (second (nth 1 fragments))
           (third (nth 2 fragments)))
      (should
       (equal
        (shuying-org-source-in-ranges
         (list
          (cons (shuying-org-formula-beginning third)
                (shuying-org-formula-end third))
          (cons (shuying-org-formula-beginning first)
                (shuying-org-formula-end first))))
        (list first third)))
      (should
       (equal
        (shuying-org-source-in-ranges
         (list (cons (shuying-org-formula-end first)
                     (shuying-org-formula-beginning third))))
        (list second))))))

(ert-deftest shuying-org-source-numbers-the-whole-document-while-narrowed ()
  (with-temp-buffer
    (org-mode)
    (insert
     "\\begin{equation}\nx = y\n\\end{equation}\n\n"
     "\\begin{equation}\ny = z\n\\end{equation}\n")
    (goto-char (point-max))
    (search-backward "y = z")
    (let ((position (point)))
      (save-restriction
        (search-backward "\\begin{equation}")
        (narrow-to-region (point) (point-max))
        (goto-char position)
        (let ((narrowed-start (point-min)))
          (should-not
           (shuying-org-formula-equation-number
            (shuying-org-source-at-position position)))
          (should
           (equal
            (mapcar #'shuying-org-formula-equation-number
                    (shuying-org-source-formulas))
            '(1 2)))
          (should (shuying-org-source-current-p))
          (should (= (point) position))
          (should (= (point-min) narrowed-start))
          (shuying-org-source-reset)
          (should-not (shuying-org-source-current-p)))))))

(ert-deftest shuying-org-source-loads-without-renderer ()
  (yunge-test-run-emacs
   "-l" "shuying-org-source"
   "--eval"
   (prin1-to-string
    '(when (or (featurep 'shuying-org)
               (featurep 'shuying)
               (featurep 'shuying-latex))
       (error "Loading Org sources started the preview renderer")))))

(provide 'shuying-org-source-test)

;;; shuying-org-source-test.el ends here
