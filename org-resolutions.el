;;; org-resolutions.el --- Evaluate Org checkbox items from numeric rules -*- lexical-binding: t; -*-

;; Author: Robert Levy via Codex
;; Keywords: outlines, org
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:

;; Mark a parent checkbox item with `#+resolutions`:
;;
;;   - [0%] #+resolutions
;;     - [ ] bike-miles 10 8 12
;;     - [ ] read-pages 70
;;
;; Each child item is interpreted as:
;;
;;   NAME ARG1 ARG2 ...
;;
;; `NAME` is looked up in `org-resolutions-rules`.  The checkbox is set to
;; done when the rule result is strictly less than `org-resolutions-threshold`.
;;
;; Configure `org-resolutions-rules` in your init file or see `example-usage.el`
;; for a complete sample setup.
;;
;; Enable `org-resolutions-mode` to make `C-c C-c` evaluate the surrounding
;; resolutions list when point is on the marker item or one of its children.

;;; Code:

(require 'cl-lib)
(require 'org)

(defgroup org-resolutions nil
  "Evaluate Org checkbox lists from named numeric rules."
  :group 'org)

(defcustom org-resolutions-marker "#+resolutions"
  "Marker string used in a parent list item to identify a resolutions list."
  :type 'string
  :group 'org-resolutions)

(defcustom org-resolutions-threshold 1
  "Checkboxes are set to done when a rule returns a value below this threshold."
  :type 'number
  :group 'org-resolutions)

(defcustom org-resolutions-rules
  nil
  "Alist mapping item names to functions.

Each function receives a list of numeric arguments and returns a number.
The item is checked when that number is less than
`org-resolutions-threshold`.

See `example-usage.el` for a sample configuration."
  :type '(alist :key-type string :value-type function)
  :group 'org-resolutions)

(defconst org-resolutions--checkbox-regexp
  "^\\([ \t]*[-+*]\\(?:[ \t]+\\)\\)\\(\\[[ X-]\\]\\)\\([ \t]+.*\\)$")

(defun org-resolutions--item-line ()
  "Return the current item line as a string, or nil when not on an item."
  (save-excursion
    (when (org-at-item-p)
      (org-beginning-of-item)
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun org-resolutions--marker-item-pos ()
  "Return the nearest ancestor item position with the resolutions marker."
  (save-excursion
    (beginning-of-line)
    (let ((start-indent (current-indentation))
          (first-line t)
          found)
      (while (not found)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (when (and (string-match-p "^[ \t]*[-+*][ \t]+" line)
                     (string-match-p (regexp-quote org-resolutions-marker) line)
                     (or first-line (< (current-indentation) start-indent)))
            (setq found (line-beginning-position))))
        (setq first-line nil)
        (unless found
          (if (bobp)
              (setq found :none)
            (forward-line -1))))
      (unless (eq found :none)
        found))))

(defun org-resolutions--number (string)
  "Parse STRING as a number, signaling a user error on failure."
  (cond
   ((string-match-p "\\`[-+]?[0-9]+\\'" string)
    (string-to-number string))
   ((string-match-p "\\`[-+]?[0-9]*\\.[0-9]+\\'" string)
    (string-to-number string))
   (t
    (user-error "org-resolutions: invalid numeric argument %S" string))))

(defun org-resolutions--parse-child-line (line)
  "Parse a child item LINE into a plist."
  (when (string-match org-resolutions--checkbox-regexp line)
    (let* ((content (string-trim (match-string 3 line)))
           (parts (split-string content "[ \t]+" t)))
      (when parts
        (list :name (car parts)
              :args (mapcar #'org-resolutions--number (cdr parts)))))))

(defun org-resolutions--set-checkbox (checked)
  "Set the checkbox on the current item line to CHECKED."
  (save-excursion
    (org-beginning-of-item)
    (let ((line (org-resolutions--item-line)))
      (unless (and line (string-match org-resolutions--checkbox-regexp line))
        (user-error "org-resolutions: expected a checkbox item"))
      (setq line
            (concat (match-string 1 line)
                    (if checked "[X]" "[ ]")
                    (match-string 3 line)))
      (delete-region (line-beginning-position) (line-end-position))
      (insert line))))

(defun org-resolutions--update-cookie (marker-pos)
  "Refresh the checkbox cookie for the list identified by MARKER-POS."
  (save-excursion
    (goto-char marker-pos)
    (when (fboundp 'org-update-checkbox-count)
      (org-update-checkbox-count t))))

(defun org-resolutions--resolve-child-at-point ()
  "Evaluate the child item at point and update its checkbox."
  (let* ((line (or (org-resolutions--item-line)
                   (user-error "org-resolutions: point is not on a list item")))
         (spec (or (org-resolutions--parse-child-line line)
                   (user-error "org-resolutions: item does not match NAME ARG... syntax")))
         (rule (alist-get (plist-get spec :name) org-resolutions-rules nil nil #'string=)))
    (unless rule
      (user-error "org-resolutions: no rule configured for %S" (plist-get spec :name)))
    (org-resolutions--set-checkbox
     (< (funcall rule (plist-get spec :args)) org-resolutions-threshold))))

(defun org-resolutions--child-item-positions (marker-pos)
  "Return the positions of immediate child items under MARKER-POS."
  (save-excursion
    (goto-char marker-pos)
    (org-beginning-of-item)
    (let* ((parent-indent (current-indentation))
           (end (save-excursion (org-end-of-item) (point)))
           positions
           child-indent)
      (forward-line 1)
      (while (< (point) end)
        (cond
         ((looking-at "^[ \t]*$")
          (forward-line 1))
         ((not (looking-at "^[ \t]*[-+*][ \t]+\\[[ X-]\\]"))
          (forward-line 1))
         (t
          (let ((indent (current-indentation)))
            (cond
             ((<= indent parent-indent)
              (goto-char end))
             ((null child-indent)
              (setq child-indent indent)
              (push (point) positions)
              (forward-line 1))
             ((= indent child-indent)
              (push (point) positions)
              (forward-line 1))
             (t
              (forward-line 1)))))))
      (nreverse positions))))

;;;###autoload
(defun org-resolutions-apply ()
  "Evaluate the current resolutions list and update child checkboxes."
  (interactive)
  (let ((marker-pos (or (org-resolutions--marker-item-pos)
                        (user-error "org-resolutions: not inside a %s list"
                                    org-resolutions-marker))))
    (save-excursion
      (dolist (pos (org-resolutions--child-item-positions marker-pos))
        (goto-char pos)
        (org-resolutions--resolve-child-at-point)))
    (org-resolutions--update-cookie marker-pos)
    (message "org-resolutions: updated %d items"
             (length (org-resolutions--child-item-positions marker-pos)))))

(defun org-resolutions--ctrl-c-ctrl-c-hook ()
  "Handle `org-ctrl-c-ctrl-c' inside resolutions lists."
  (when (and (derived-mode-p 'org-mode)
             (org-at-item-p)
             (org-resolutions--marker-item-pos))
    (org-resolutions-apply)
    t))

;;;###autoload
(define-minor-mode org-resolutions-mode
  "Evaluate Org resolutions lists from `org-ctrl-c-ctrl-c'."
  :global t
  :group 'org-resolutions
  (if org-resolutions-mode
      (add-hook 'org-ctrl-c-ctrl-c-hook #'org-resolutions--ctrl-c-ctrl-c-hook)
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-resolutions--ctrl-c-ctrl-c-hook)))

(provide 'org-resolutions)

;;; org-resolutions.el ends here
