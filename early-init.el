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

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq use-package-always-ensure t)

(setq package-native-compile t
	  native-comp-async-report-warnings-errors nil)


;; Minimal UI
;; ==========
(setq-default
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
