;;; org-stable-ids-tests.el --- Tests for org-stable-ids  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;
;;   emacs --batch -L . -l org-stable-ids-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org-stable-ids)

(ert-deftest org-stable-ids-test-slugify-examples ()
  "The slugs shown in the README and the `org-stable-ids--slugify' docstring."
  (let ((case-fold-search t))
    (dolist (case '(("Crème brûlée"      . "creme-brulee")
                    ("ação à pé"         . "acao-a-pe")
                    ("zażółć gęślą"      . "zazolc-gesla")
                    ("zażółć gęślą jaźń" . "zazolc-gesla-jazn")
                    ("Привет мир"        . "privet-mir")
                    ("init.el"           . "init-el")
                    ("γειά σου"          . "geia-soy")))
      (should (equal (org-stable-ids--slugify (car case)) (cdr case))))))

(ert-deftest org-stable-ids-test-greek-iota ()
  "Iota survives even though U+0345 case-folds to it."
  (let ((case-fold-search t))
    (should (equal (org-stable-ids--slugify "ι") "i"))
    (should (equal (org-stable-ids--slugify "Ἰησοῦς") "iesoys"))))

(ert-deftest org-stable-ids-test-slugify-blank ()
  (should-not (org-stable-ids--slugify nil))
  (should-not (org-stable-ids--slugify "  "))
  (should-not (org-stable-ids--slugify "!!!")))

(provide 'org-stable-ids-tests)
;;; org-stable-ids-tests.el ends here
