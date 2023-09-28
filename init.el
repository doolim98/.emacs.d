;; Config my load path
(let ((default-directory (concat user-emacs-directory "/lisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory (concat user-emacs-directory "/third-party/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(straight-use-package 'org)

;; Mac OS
(when (eq system-type 'darwin)
  (setq-default org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org")
  (setq mac-command-modifier 'meta
		mac-option-modifier 'super))

(require 'config-emacs)
(require 'config-minimal-packages)
(require 'config-keybindings)
(require 'my)
(require 'config-project)
(require 'config-org)
(require 'config-prog-mode)
(require 'config-completion)
(require 'config-appearance)


