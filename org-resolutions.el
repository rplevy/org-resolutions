;;; org-resolutions.el --- Evaluate Org checkbox items from numeric rules -*- lexical-binding: t; -*-

;; Author: Robert Levy via Codex
;; Keywords: outlines, org
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:

;; Mark a parent checkbox item with `#+resolutions`:
;;
;;   - [33%] #+resolutions
;;     - [27%] drink-water 4 5 4
;;     - [48%] bike-miles 13 16
;;     - [100%] read-pages 70
;;
;; Each child item is interpreted as:
;;
;;   NAME ARG1 ARG2 ...
;;
;; `NAME` is looked up in `org-resolutions-rules`.
;;
;; A rule may return either:
;;
;; - a number, preserving the original behavior where the item is marked done
;;   when the value is strictly less than `org-resolutions-threshold`
;; - a list (GOAL CURRENT), which renders the item cookie as a percentage based
;;   on CURRENT / GOAL and treats the item as complete once it reaches 100%
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
  "Legacy threshold used when a rule returns a single number."
  :type 'number
  :group 'org-resolutions)

(defcustom org-resolutions-rules
  nil
  "Alist mapping item names to functions.

Each function receives a list of numeric arguments.

Functions may return either a number or a list of the form (GOAL CURRENT).
When a number is returned, the original threshold behavior is used and the
item becomes either 0% or 100%.  When a list is returned, the item cookie is
set to the computed percentage and completion is based on whether CURRENT has
reached GOAL.

See `example-usage.el` for a sample configuration."
  :type '(alist :key-type string :value-type function)
  :group 'org-resolutions)

(defconst org-resolutions--checkbox-regexp
  "^\\([ \t]*[-+*]\\(?:[ \t]+\\)\\)\\(\\[[^]\n]+\\]\\)\\([ \t]+.*\\)$")

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

(defun org-resolutions--line-token (line)
  "Extract the bracket token from LINE."
  (when (string-match org-resolutions--checkbox-regexp line)
    (match-string 2 line)))

(defun org-resolutions--replace-item-token (token)
  "Replace the bracket token on the current item line with TOKEN."
  (save-excursion
    (org-beginning-of-item)
    (let ((line (org-resolutions--item-line)))
      (unless (and line (string-match org-resolutions--checkbox-regexp line))
        (user-error "org-resolutions: expected a checkbox item"))
      (setq line
            (concat (match-string 1 line)
                    token
                    (match-string 3 line)))
      (delete-region (line-beginning-position) (line-end-position))
      (insert line))))

(defun org-resolutions--progress-percent (goal current)
  "Return completion percentage for GOAL and CURRENT, clamped to 0..100."
  (cond
   ((>= current goal) 100)
   ((<= goal 0) 0)
   (t
    (max 0 (min 100 (floor (* 100.0 (/ (float current) goal))))))))

(defun org-resolutions--normalize-result (result)
  "Normalize rule RESULT into a plist with :percent and :complete."
  (cond
   ((numberp result)
    (let ((complete (< result org-resolutions-threshold)))
      (list :percent (if complete 100 0)
            :complete complete)))
   ((and (listp result)
         (= (length result) 2)
         (numberp (nth 0 result))
         (numberp (nth 1 result)))
    (let* ((goal (nth 0 result))
           (current (nth 1 result))
           (percent (org-resolutions--progress-percent goal current)))
      (list :percent percent
            :complete (= percent 100))))
   (t
    (user-error
     "org-resolutions: rule must return a number or (GOAL CURRENT), got %S"
     result))))

(defun org-resolutions--set-item-percent (percent)
  "Set the current item cookie to PERCENT."
  (org-resolutions--replace-item-token (format "[%d%%]" percent)))

(defun org-resolutions--set-marker-percent (marker-pos complete total)
  "Set the marker item cookie at MARKER-POS from COMPLETE and TOTAL children."
  (save-excursion
    (goto-char marker-pos)
    (org-resolutions--replace-item-token
     (format "[%d%%]"
             (if (> total 0)
                 (floor (* 100.0 (/ (float complete) total)))
               0)))))

(defun org-resolutions--resolve-child-at-point ()
  "Evaluate the child item at point and update its progress cookie."
  (let* ((line (or (org-resolutions--item-line)
                   (user-error "org-resolutions: point is not on a list item")))
         (spec (or (org-resolutions--parse-child-line line)
                   (user-error "org-resolutions: item does not match NAME ARG... syntax")))
         (rule (alist-get (plist-get spec :name) org-resolutions-rules nil nil #'string=))
         (result nil))
    (unless rule
      (user-error "org-resolutions: no rule configured for %S" (plist-get spec :name)))
    (setq result (org-resolutions--normalize-result
                  (funcall rule (plist-get spec :args))))
    (org-resolutions--set-item-percent (plist-get result :percent))
    result))

(defun org-resolutions--child-item-positions (marker-pos)
  "Return markers for immediate child items under MARKER-POS."
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
         ((not (looking-at "^[ \t]*[-+*][ \t]+\\[[^]\n]+\\]"))
          (forward-line 1))
         (t
          (let ((indent (current-indentation)))
            (cond
             ((<= indent parent-indent)
              (goto-char end))
             ((null child-indent)
              (setq child-indent indent)
              (push (copy-marker (point)) positions)
              (forward-line 1))
             ((= indent child-indent)
              (push (copy-marker (point)) positions)
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
                                    org-resolutions-marker)))
        (complete-count 0)
        (positions nil))
    (setq positions (org-resolutions--child-item-positions marker-pos))
    (save-excursion
      (dolist (pos positions)
        (goto-char pos)
        (when (plist-get (org-resolutions--resolve-child-at-point) :complete)
          (setq complete-count (1+ complete-count)))))
    (org-resolutions--set-marker-percent marker-pos complete-count (length positions))
    (message "org-resolutions: updated %d items"
             (length positions))))

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
