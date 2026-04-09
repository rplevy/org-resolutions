;;; example-usage.el --- Example configuration for org-resolutions -*- lexical-binding: t; -*-

;;; Commentary:

;; Load the library, configure your rules, and enable the mode:
;;
;;   (load-file "/path/to/org-resolutions.el")
;;   (load-file "/path/to/example-usage.el")
;;
;; In an Org buffer, use a marker item like:
;;
;;   - [33%] #+resolutions
;;     - [27%] drink-water 4 5 4
;;     - [48%] bike-miles 13 16
;;     - [100%] read-pages 70

;;; Code:

(require 'org-resolutions)

(setq org-resolutions-rules
      '(("drink-water" . (lambda (args) (list 48 (apply #'+ args))))
        ("bike-miles" . (lambda (args) (list 60 (apply #'+ args))))
        ("read-pages" . (lambda (args) (list 60 (apply #'+ args))))
        ("personal-coding-hours" . (lambda (args) (list 20 (apply #'+ args))))
        ("son-reads-books" . (lambda (args) (list 6 (apply #'+ args))))
        ("son-practices-math" . (lambda (args) (list 6 (apply #'+ args))))))

(org-resolutions-mode 1)

(provide 'example-usage)

;;; example-usage.el ends here
