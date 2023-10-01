;; Package
;; =======

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;; Use-package
(unless (package-installed-p 'use-package)
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
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
