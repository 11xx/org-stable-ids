;;; org-stable-ids.el --- Human-readable ASCII slug IDs for Org headings and export  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Version: 20260406
;; Keywords: outlines, hypermedia, text
;; URL: https://codeberg.org/useless-utils/org-stable-ids

;;; Commentary:

;; Provides two composable entry points:
;;
;;   `org-stable-ids-get-create' — interactive command that assigns a
;;     human-readable :CUSTOM_ID: to the current entry heading, derived from
;;     the heading title.  Collisions are resolved by prepending ancestor
;;     heading slugs (nearest first), then by a numeric suffix.
;;
;;   `org-stable-ids-enable' — activates an around-advice on
;;     `org-export-get-reference' so that ox-html (and derived backends) emit
;;     deterministic, readable, ASCII slug-based fragment IDs.  Pre-existing
;;     :CUSTOM_ID: values are honoured verbatim; <<targets>>, named tables, and
;;     list-item targets are also handled.
;;
;; Slug generation transliterates selected Latin, Greek, and Cyrillic
;; characters toward ASCII, removes combining diacritics, lowercases,
;; and replaces runs of non-alphanumeric characters with a separator.
;;
;; Example usage:
;;
;;    (use-package org-stable-ids
;;      :vc (:url "https://codeberg.org/useless-utils/org-stable-ids")
;;      :init
;;      (keymap-global-set "C-c o i" #'org-stable-ids-get-create)
;;      :config
;;      (org-stable-ids-enable))

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'ucs-normalize)

;;;; Customization

;;;###autoload
(defgroup org-stable-ids nil
  "Stable, ASCII slug-based identifiers for Org headings and export."
  :group 'org-export
  :prefix "org-stable-ids-"
  :link '(url-link :tag "Codeberg" "https://codeberg.org/useless-utils/org-stable-ids"))

(defun org-stable-ids--non-empty-string-p (s)
  "Return non-nil when S is a non-empty string."
  (and (stringp s) (not (string-empty-p s))))

;;;###autoload
(defcustom org-stable-ids-separator "-"
  "Token separator used within generated slugs."
  :type 'string
  :group 'org-stable-ids
  :safe #'org-stable-ids--non-empty-string-p)

;;;###autoload
(defcustom org-stable-ids-ancestor-separator "--"
  "Separator between an ancestor prefix and the base slug during disambiguation."
  :type 'string
  :group 'org-stable-ids
  :safe #'org-stable-ids--non-empty-string-p)

;;;###autoload
(defcustom org-stable-ids-store-link-after-create t
  "When non-nil, call `org-store-link' after creating or retrieving a CUSTOM_ID."
  :type 'boolean
  :group 'org-stable-ids
  :safe #'booleanp)

;;;###autoload
(defcustom org-stable-ids-require-point-at-heading nil
  "When non-nil, require point to be on a headline for `org-stable-ids-get-create'.

When nil, the command operates on the current Org entry and may be
invoked from anywhere within the entry subtree."
  :type 'boolean
  :group 'org-stable-ids
  :safe #'booleanp)

;;;###autoload
(defcustom org-stable-ids-max-slug-length 60
  "Maximum character count for a generated slug; longer slugs are truncated."
  :type 'natnum
  :group 'org-stable-ids
  :safe #'natnump)

;;;; Stage 1 — Transliteration and slugification

(defconst org-stable-ids--combining-marks-re
  "[\u0300-\u036f\u1ab0-\u1aff\u1dc0-\u1dff\u20d0-\u20ff\uFE20-\uFE2F]"
  "Regexp matching common Unicode combining-mark ranges.

Intended for use when diacritic marks should be removed from decomposed
Unicode text. In normalization forms such as NFD, many accented
characters are represented as a base character followed by one or more
combining marks. Removing those marks yields a simplified base-letter
representation suitable for tasks such as slug generation.

The covered ranges are:

  U+0300-U+036F  Combining Diacritical Marks
    Primary block containing many commonly used combining accents and
    related marks across multiple scripts.

  U+1AB0-U+1AFF  Combining Diacritical Marks Extended
    Additional combining marks beyond the primary block.

  U+1DC0-U+1DFF  Combining Diacritical Marks Supplement
    Supplementary combining marks, including less common and specialized
    forms.

  U+20D0-U+20FF  Combining Diacritical Marks for Symbols
    Combining marks associated with symbol characters.

  U+FE20-U+FE2F  Combining Half Marks
    Half combining marks used in certain decomposed sequences.

This is a heuristic intended to cover the combining-mark ranges most
commonly relevant to text simplification. It is not a complete model of
Unicode grapheme structure or script-specific orthographic behavior.")

(defconst org-stable-ids--ascii-transliteration-map
  '(
    ;; Latin special letters and ligatures.
    ("ß" . "ss")
    ("æ" . "ae")
    ("œ" . "oe")
    ("ø" . "o")
    ("å" . "a")
    ("þ" . "th")
    ("ð" . "d")
    ("đ" . "d")
    ("ł" . "l")
    ("ħ" . "h")
    ("ŧ" . "t")
    ("ŋ" . "ng")
    ("ĳ" . "ij")
    ("ŉ" . "n")
    ("ſ" . "s")
    ("ƒ" . "f")
    ("ĸ" . "k")
    ("ı" . "i")
    ("ə" . "e")

    ;; Greek lowercase.
    ("α" . "a")   ("β" . "b")   ("γ" . "g")   ("δ" . "d")
    ("ε" . "e")   ("ζ" . "z")   ("η" . "e")   ("θ" . "th")
    ("ι" . "i")   ("κ" . "k")   ("λ" . "l")   ("μ" . "m")
    ("ν" . "n")   ("ξ" . "x")   ("ο" . "o")   ("π" . "p")
    ("ρ" . "r")   ("σ" . "s")   ("ς" . "s")   ("τ" . "t")
    ("υ" . "y")   ("φ" . "ph")  ("χ" . "ch")  ("ψ" . "ps")
    ("ω" . "o")

    ;; Cyrillic lowercase.
    ("а" . "a")   ("б" . "b")   ("в" . "v")   ("г" . "g")
    ("д" . "d")   ("е" . "e")   ("ё" . "e")   ("ж" . "zh")
    ("з" . "z")   ("и" . "i")   ("й" . "i")   ("к" . "k")
    ("л" . "l")   ("м" . "m")   ("н" . "n")   ("о" . "o")
    ("п" . "p")   ("р" . "r")   ("с" . "s")   ("т" . "t")
    ("у" . "u")   ("ф" . "f")   ("х" . "kh")  ("ц" . "ts")
    ("ч" . "ch")  ("ш" . "sh")  ("щ" . "shch")
    ("ъ" . "")    ("ы" . "y")   ("ь" . "")
    ("э" . "e")   ("ю" . "yu")  ("я" . "ya"))
  "Character-to-string transliterations used for slug generation.

These mappings are slug-oriented rather than linguistically exact.  They
favor readability, stability, and broad familiarity over strict adherence
to any single national transliteration standard.

Only lowercase mappings are listed here because slug generation
lowercases text before table-based transliteration.")

(defvar org-stable-ids--ascii-transliteration-table nil
  "Hash table built from `org-stable-ids--ascii-transliteration-map'.")

(defun org-stable-ids--ensure-ascii-transliteration-table ()
  "Initialize `org-stable-ids--ascii-transliteration-table' if needed."
  (unless org-stable-ids--ascii-transliteration-table
    (let ((tbl (make-hash-table :test #'equal)))
      (dolist (pair org-stable-ids--ascii-transliteration-map)
        (puthash (car pair) (cdr pair) tbl))
      (setq org-stable-ids--ascii-transliteration-table tbl))))

(defun org-stable-ids--replace-chars-from-ascii-table (s)
  "Return S with characters replaced via the ASCII transliteration table."
  (org-stable-ids--ensure-ascii-transliteration-table)
  (mapconcat
   (lambda (ch)
     (or (gethash (char-to-string ch) org-stable-ids--ascii-transliteration-table)
         (char-to-string ch)))
   s ""))

(defun org-stable-ids--transliterate-to-ascii (s)
  "Return S transliterated toward ASCII for slug generation.

This function first lowercases S, then applies Unicode NFD
normalization and removes combining diacritical marks.  That
reduces many accented Latin letters to their plain ASCII bases,
for example \"é\" → \"e\" and \"ã\" → \"a\".

It then applies explicit transliteration mappings for selected
letters from Latin, Greek, and Cyrillic scripts that are not
handled sufficiently by normalization alone, for example
\"ß\" → \"ss\", \"θ\" → \"th\", and \"ж\" → \"zh\".

The result is intended as input to `org-stable-ids--slugify',
which performs the final replacement of non-alphanumeric runs
with `org-stable-ids-separator'."
  (when (stringp s)
    (setq s (downcase s))
    (setq s (ucs-normalize-NFD-string s))
    (setq s (replace-regexp-in-string org-stable-ids--combining-marks-re "" s))
    (org-stable-ids--replace-chars-from-ascii-table s)))

(defun org-stable-ids--slugify (s)
  "Return a stable, lowercase ASCII slug derived from S, or nil for blank input.

Slug generation proceeds in four stages:

1. Transliterate S toward ASCII with
   `org-stable-ids--transliterate-to-ascii'.
2. Replace each run of non-alphanumeric characters with
   `org-stable-ids-separator'.
3. Collapse repeated separators and trim separators at both ends.
4. Truncate the result to `org-stable-ids-max-slug-length'.

The intent is to produce readable and export-friendly identifiers such as:

  \"Crème brûlée\"   → \"creme-brulee\"
  \"zażółć gęślą\"   → \"zazolc-gesla\"
  \"Привет мир\"     → \"privet-mir\"
  \"γειά σου\"       → \"geia-soy\"

If no usable slug can be produced, return nil."
  (when (and (stringp s) (string-match-p (rx (not space)) s))
    (let* ((sep    org-stable-ids-separator)
           (sep-re (regexp-quote sep))
           (slug   (org-stable-ids--transliterate-to-ascii s))
           (slug   (replace-regexp-in-string "[^[:alnum:]]+" sep slug))
           (slug   (replace-regexp-in-string (concat sep-re "+") sep slug))
           (slug   (string-trim slug
                                (concat sep-re "+")
                                (concat sep-re "+"))))
      (when (org-string-nw-p slug)
        ;; Trim separators again because truncation may end in a separator
        (setq slug (if (> (length slug) org-stable-ids-max-slug-length)
                       (substring slug 0 org-stable-ids-max-slug-length)
                     slug))
        (setq slug (string-trim slug (concat sep-re "+") (concat sep-re "+")))
        (and (org-string-nw-p slug) slug)))))

;;;; Stage 2 — Ancestor traversal (pure)

(defun org-stable-ids--ancestors-from-element (datum)
  "Return ancestor headline raw-values for element DATUM, nearest first.
Relies on `:parent' links being set — available during export tree traversal."
  (let (acc cur)
    (setq cur datum)
    (while (setq cur (org-element-property :parent cur))
      (when (eq (org-element-type cur) 'headline)
        (push (org-element-property :raw-value cur) acc)))
    (nreverse acc)))

(defun org-stable-ids--cache-key (datum)
  "Return a path string uniquely identifying headline DATUM within its tree."
  (let ((ancestors (org-stable-ids--ancestors-from-element datum))
        (title     (or (org-element-property :raw-value datum) "")))
    (mapconcat #'identity (append ancestors (list title)) "/")))

;;;; Stage 3 — Disambiguation (stateful against a hash-table)

(defun org-stable-ids--resolve (base-id ancestors used-table
                                        &optional cache-table cache-key)
  "Return a unique variant of BASE-ID not yet recorded in USED-TABLE.

ANCESTORS is a list of heading strings ordered nearest-first and is used
for contextual disambiguation before falling back to numeric suffixes.

CACHE-TABLE and CACHE-KEY enable consistent re-resolution so that
multiple export calls for the same logical heading, such as TOC and
body entries, receive the same fragment identifier."
  (cl-block nil
    (when (and cache-table cache-key)
      (when-let* ((hit (gethash cache-key cache-table)))
        (puthash hit t used-table)
        (cl-return hit)))
    (let ((final-id
           (cond
            ((not (gethash base-id used-table))
             base-id)
            ((cl-loop for anc       in ancestors
                      for prefix    = (org-stable-ids--slugify anc)
                      for candidate = (when prefix
                                        (concat prefix
                                                org-stable-ids-ancestor-separator
                                                base-id))
                      when (and candidate (not (gethash candidate used-table)))
                      return candidate))
            (t
             (cl-loop for n from 2
                      for candidate = (format "%s-%d" base-id n)
                      unless (gethash candidate used-table)
                      return candidate)))))
      (puthash final-id t used-table)
      (when (and cache-table cache-key)
        (puthash cache-key final-id cache-table))
      final-id)))

(defun org-stable-ids--ensure-command-entry ()
  "Signal a `user-error' when `org-stable-ids-get-create' has no valid entry.

When `org-stable-ids-require-point-at-heading' is non-nil, point must
already be on a headline.  Otherwise, point may be anywhere within the
current Org entry subtree."
  (if org-stable-ids-require-point-at-heading
      (unless (org-at-heading-p)
        (user-error "Point is not on an Org heading"))
    (condition-case nil
        (org-back-to-heading t)
      (error
       (user-error "Point is not inside an Org heading")))))

;;;; Stage 4a — Interactive :CUSTOM_ID: assignment

(defun org-stable-ids--buffer-used-table ()
  "Return a hash table of all :CUSTOM_ID: values in the current buffer."
  (let ((tbl (make-hash-table :test #'equal)))
    (org-map-entries
     (lambda ()
       (when-let* ((id (org-entry-get nil "CUSTOM_ID")))
         (puthash id t tbl))))
    tbl))

;;;###autoload
(defun org-stable-ids-get-create (&optional force)
  "Get or create a slug :CUSTOM_ID: for the current entry heading.

With universal prefix argument FORCE non-nil, always regenerate the
identifier even if one already exists.

The slug derives from the heading title.  Collisions are resolved by
prepending ancestor slugs, nearest first, and then by a numeric suffix.

This command does not register entries in `org-id-locations';
`org-store-link' already handles :CUSTOM_ID: links natively."
  (interactive "P")
  (let (result)
    (save-excursion
      (org-stable-ids--ensure-command-entry)
      (let* ((heading    (org-get-heading t t t t))
             (current-id (org-entry-get nil "CUSTOM_ID")))
        (setq result
              (if (and (not force) (org-string-nw-p current-id))
                  (progn
                    (when org-stable-ids-store-link-after-create
                      (org-store-link nil t))
                    current-id)
                (let* ((base      (or (org-stable-ids--slugify heading)
                                      (format "heading-%s"
                                              (substring (md5 (or heading "")) 0 6))))
                       (ancestors (nreverse (org-get-outline-path)))
                       (used-tbl  (let ((tbl (org-stable-ids--buffer-used-table)))
                                    (when (org-string-nw-p current-id)
                                      (remhash current-id tbl))
                                    tbl))
                       (new-id    (org-stable-ids--resolve base ancestors used-tbl)))
                  (org-set-property "CUSTOM_ID" new-id)
                  (when org-stable-ids-store-link-after-create
                    (org-store-link nil t))
                  new-id)))))
    (when result
      (message "CUSTOM_ID: %s" result))
    result))

;;;; Stage 4b — Export stable-ID advice

(defvar org-stable-ids--used nil
  "Hash-table tracking IDs generated during the current export pass.")

(defvar org-stable-ids--cache nil
  "Hash-table mapping headline cache-keys to resolved IDs for the current export.")

(defun org-stable-ids--export-reset (&rest _)
  "Reset per-export ID tables.
This function is intended for `org-export-before-processing-functions'."
  (setq org-stable-ids--used  (make-hash-table :test #'equal)
        org-stable-ids--cache (make-hash-table :test #'equal)))

(defun org-stable-ids--first-target (item info)
  "Return the first target or radio-target inside list ITEM, or nil."
  (org-element-map (org-element-contents item) '(target radio-target)
    #'identity info 'first-match))

(defun org-stable-ids--get-reference (orig datum info)
  "Around-advice for `org-export-get-reference' producing stable slug IDs.

Dispatch by element type:

  Headline  → existing :CUSTOM_ID:, else slug of heading title
  Target    → slug of target value
  List item → slug of embedded <<target>>, else ORIG
  Table     → slug of #+NAME:, else ORIG
  Other     → ORIG"
  (pcase (org-element-type datum)
    ('headline
     (let* ((custom (org-element-property :CUSTOM_ID datum))
            (base   (or (and (org-string-nw-p custom) custom)
                        (org-stable-ids--slugify
                         (org-element-property :raw-value datum)))))
       (if base
           (org-stable-ids--resolve
            base
            (org-stable-ids--ancestors-from-element datum)
            org-stable-ids--used
            org-stable-ids--cache
            (org-stable-ids--cache-key datum))
         (funcall orig datum info))))

    ((or 'target 'radio-target)
     (let* ((raw   (org-element-property :value datum))
            (clean (and raw (replace-regexp-in-string "[<>]" "" raw)))
            (base  (org-stable-ids--slugify clean)))
       (if base
           (org-stable-ids--resolve
            base
            (org-stable-ids--ancestors-from-element datum)
            org-stable-ids--used)
         (funcall orig datum info))))

    ('item
     (if-let* ((tgt   (org-stable-ids--first-target datum info))
               (raw   (org-element-property :value tgt))
               (clean (replace-regexp-in-string "[<>]" "" raw))
               (base  (org-stable-ids--slugify clean)))
         (org-stable-ids--resolve
          base
          (org-stable-ids--ancestors-from-element datum)
          org-stable-ids--used)
       (funcall orig datum info)))

    ('table
     (if-let* ((name (org-element-property :name datum))
               (base (org-stable-ids--slugify name)))
         (org-stable-ids--resolve
          base
          (org-stable-ids--ancestors-from-element datum)
          org-stable-ids--used)
       (funcall orig datum info)))

    (_
     (funcall orig datum info))))

;;;; Setup / teardown

;;;###autoload
(defun org-stable-ids-enable ()
  "Activate the stable-ID export advice and pre-export reset hook.

Call this from a `with-eval-after-load' block for `ox' so that
the export library is guaranteed to be available."
  (require 'ox)
  (add-hook 'org-export-before-processing-functions
            #'org-stable-ids--export-reset)
  (unless (advice-member-p #'org-stable-ids--get-reference
                           'org-export-get-reference)
    (advice-add 'org-export-get-reference
                :around #'org-stable-ids--get-reference
                '((depth . -95)))))

;;;###autoload
(defun org-stable-ids-disable ()
  "Deactivate the stable-ID export advice and pre-export reset hook."
  (remove-hook 'org-export-before-processing-functions
               #'org-stable-ids--export-reset)
  (advice-remove 'org-export-get-reference
                 #'org-stable-ids--get-reference)
  (setq org-stable-ids--used nil
        org-stable-ids--cache nil))

(provide 'org-stable-ids)
;;; org-stable-ids.el ends here
