(setq my-emacs-directory user-emacs-directory)
(setq-default user-emacs-directory (concat my-emacs-directory "/.user-emacs/"))
(shell-command (format "mkdir -p %s" user-emacs-directory))

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup t)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq package-native-compile t
	  native-comp-async-report-warnings-errors nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Add lisp/ into the load-path
(let ((default-directory (concat my-emacs-directory "./lisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
