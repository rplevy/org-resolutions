;;; example-usage.el --- Example configuration for org-resolutions -*- lexical-binding: t; -*-

;;; Commentary:

;; Load the library, configure your rules, and enable the mode:
;;
;;   (load-file "/path/to/org-resolutions.el")
;;   (load-file "/path/to/example-usage.el")
;;
;; In an Org buffer, use a marker item like:
;;
;;   - [0%] #+resolutions
;;     - [ ] bike-miles 10 8 12
;;     - [ ] read-pages 70

;;; Code:

(require 'org-resolutions)

(setq org-resolutions-rules
      '(("bike-miles" . (lambda (args) (- 60 (apply #'+ args))))
        ("read-pages" . (lambda (args) (- 60 (apply #'+ args))))
        ("personal-coding-hours" . (lambda (args) (- 20 (apply #'+ args))))
        ("son-reads-books" . (lambda (args) (- 6 (apply #'+ args))))
        ("son-practices-math" . (lambda (args) (- 6 (apply #'+ args)))))
      org-resolutions-threshold 1)

(org-resolutions-mode 1)

(provide 'example-usage)

;;; example-usage.el ends here
