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

(defun org-stable-ids-test--html (org)
  "Export ORG to an HTML body string with stable IDs enabled."
  (require 'ox-html)
  (org-stable-ids-enable)
  (unwind-protect
      (with-temp-buffer
        (insert org)
        (org-mode)
        (org-export-as 'html nil nil t '(:with-toc t :section-numbers nil)))
    (org-stable-ids-disable)))

(defun org-stable-ids-test--attrs (attr html)
  "Return the values of ATTR in HTML, in order."
  (let ((re (format "%s=\"\\([^\"]*\\)\"" attr)) (pos 0) acc)
    (while (string-match re html pos)
      (push (match-string 1 html) acc)
      (setq pos (match-end 0)))
    (nreverse acc)))

(ert-deftest org-stable-ids-test-export-link-matches-target ()
  "A link to a <<target>> points at the anchor the target receives."
  (let* ((html (org-stable-ids-test--html
                "* Intro\nSee [[foo]].\n* Other\nText <<foo>> here.\n"))
         (ids (org-stable-ids-test--attrs "id" html)))
    (should (member "#foo" (org-stable-ids-test--attrs "href" html)))
    (should (member "foo" ids))))

(ert-deftest org-stable-ids-test-export-same-title-siblings ()
  "Sibling headings with the same title receive distinct anchors."
  (let* ((html (org-stable-ids-test--html "* Top\n** Notes\na\n** Notes\nb\n"))
         (ids (org-stable-ids-test--attrs "id" html)))
    (should (member "notes" ids))
    (should (member "top--notes" ids))
    (should (equal (org-stable-ids-test--attrs "href" html)
                   '("#top" "#notes" "#top--notes")))))

(provide 'org-stable-ids-tests)
;;; org-stable-ids-tests.el ends here
