;; Keep my .emacs.d clean
(setq-default my-emacs-directory user-emacs-directory)
(setq-default user-emacs-directory (expand-file-name ".user-emacs/" user-emacs-directory))
(shell-command (format "mkdir -p %s" user-emacs-directory))

(setq-default my/use-straight nil)

(when my/use-straight
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
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t))

(unless my/use-straight
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (when (not package-archive-contents) (package-refresh-contents))
  (setq use-package-always-ensure t))

(require 'use-package)
(setq use-package-enable-imenu-support t)
(setq package-enable-at-startup t)
(setq use-package-always-ensure t)
(setq package-native-compile t
	  native-comp-async-report-warnings-errors nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
