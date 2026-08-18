org-mode

(def "#+begin_definition" n> r> n> "#+end_definition"
     :ann "definition block")
(begin "#+begin_" (s name) n> r> n> "#+end_" name
       :ann "custom block")
