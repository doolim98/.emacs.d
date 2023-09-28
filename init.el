;; Config my load path
(let ((default-directory (concat user-emacs-directory "/lisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory (concat user-emacs-directory "/lisp/third-party/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(straight-use-package 'org)
(when (eq system-type 'darwin)
  (setq-default org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org"))

;; load project related configurations
(defun my/load (path)
  (load (concat user-emacs-directory path)))

(my/load "lisp/config-project.el")

(use-package repeat :defer 11
  :init (repeat-mode +1))

;; Basic packages
(use-package general)
(use-package avy)
(use-package crux)
(use-package expand-region)
(use-package avy)

(require 'config-emacs)
(require 'config-keybindings)
(require 'my)
(require 'config-appearance)
(require 'config-project)
(require 'config-org)
(require 'config-prog-mode)
(require 'config-completion)

(use-package ace-window
  :commands (aw-flip-window)
  :init
  (setq aw-background nil)
  (setq aw-dispatch-always nil)
  (setq aw-frame-offset '(50 . 50))
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:background nil :foreground "#d00000" :weight bold :height 1.0))))))

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (which-key-mode))

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000)
  )

(use-package sqlite3)

;; https://stackoverflow.com/a/77033292
(use-package tblui)

(use-package x86-lookup :ensure t
  :bind (("C-h x" . x86-lookup))
  :init
  (defun x86-lookup-browse-pdf-preview (pdf page)
  "View PDF at PAGE file using Okular."
  (start-process "preview" nil "open" "-a" "Preview" "-p" (format "%d" page) "--" pdf))
  (setq-default x86-lookup-pdf (concat org-directory "/intel-manual.pdf"))
  (setq x86-lookup-browse-pdf-function #'x86-lookup-browse-pdf-evince))


(use-package pdf-tools
  :init
  (defun my/install-pdf-tools()
    (interactive)
    (let ((default-directory (format "%s/straight/repos/pdf-tools" straight-base-dir)))
      (async-shell-command "make -s"))))
