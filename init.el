;; Config my load path
(let ((default-directory (concat user-emacs-directory "/lisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(straight-use-package 'org)

;; load project related configurations
(defun my/load (path)
  (load (concat user-emacs-directory path)))

(my/load "lisp/config-project.el")

(use-package repeat :defer 11
  :init (repeat-mode +1))

(use-package my-keybindings :straight nil :ensure nil
  :defer 10 ;; IMPORTANT!!!
  :bind-keymap ((("C-4" . ctl-x-4-map)))
  :bind (("C-c C-w"   . fixup-whitespace)
	 ("C-M-s" . save-buffer)
	 ("M-1" . delete-other-windows)
	 ("M-2" . split-window-below)
	 ("M-3" . split-window-right)
	 ("C-x C-k" . kill-this-buffer)
	 ("C-M-e" . eshell)
	 ("C-." . er/expand-region)
	 ("M-[" . previous-buffer)
	 ("M-]" . next-buffer)
	 ("C-x t" . vterm)

	 ;; Git
	 :map global-map
	 ("C-c g c" . magit-commit)
	 ("C-c g s" . git-gutter:stage-hunk)
	 ("C-c g n" . git-gutter:next-hunk)
	 ("C-c g p" . git-gutter:previous-hunk)
	 :repeat-map git-gutter:next-hunk-map
	 ("n" . git-gutter:next-hunk)
	 ("p" . git-gutter:previous-hunk)
	 ("s" . git-gutter:stage-hunk)

	 ;; Org
	 :map global-map
	 ("C-M-y" . org-download-screenshot)

	 ;; My scroll up/down
	 ("C-d" . my/scroll-down)
	 ("C-u" . my/scroll-up)

	 ;; Avy
	 ("M-j" . avy-goto-word-0)
	 ("C-j" . avy-goto-word-1)

	 ;; Crux
	 ("C-o" . crux-smart-open-line)
	 ("M-o" . crux-other-window-or-switch-buffer)
	 ("C-x C-u" . crux-upcase-region)
	 ("C-x C-l" . crux-downcase-region)
	 ("C-x M-c" . crux-capitalize-region)

	 ;; Consult
	 ("C-x b"       . consult-buffer)
	 ("M-y"         . consult-yank-pop)
	 ("M-g g"       . consult-goto-line)
	 ("M-g M-g"     . consult-goto-line)
	 ("M-g f"       . consult-flymake)
	 ("M-g i"       . consult-imenu)
	 ("M-s l"       . consult-line)
	 ("M-s L"       . consult-line-multi)
	 ("M-s u"       . consult-focus-lines)
	 ("M-s g"       . consult-ripgrep)
	 ("M-s M-g"     . consult-ripgrep)
	 ("C-x M-:"     . consult-complex-command)
	 ("C-c n"       . consult-org-agenda)
	 ("C-c m"       . my/notegrep)

	 :map corfu-map
	 ("C-h" . corfu-popupinfo-toggle)
	 ("TAB"        . corfu-insert)
	 ([tab]        . corfu-insert)
	 ("<return>" . newline)
	 ("<escape>" . keyboard-quit)

	 :map eglot-mode-map
	 ("C-c C-q" . eglot-code-action-quickfix)
	 ("C-c C-f" . eglot-format-buffer)

	 :map help-map
	 ("a" . consult-apropos)

	 :map minibuffer-local-map
	 ("C-r" . consult-history)

	 :map dired-mode-map
	 ("-" . dired-up-directory)
	 ("." . cycle-dired-switches))
  :init
  (global-set-key (kbd "<escape>")      'keyboard-quit))

(use-package emacs :straight nil :ensure nil :defer nil
  :config
  ;; (setq-default header-line-format mode-line-format)

  (savehist-mode 1)
  (setq-default delete-pair-blink-delay 0)


  (setq blink-cursor-delay 0.0
	blink-cursor-interval 0.2
	blink-cursor-blinks 9999)
  (setq-default	cursor-type 'box)

  (setq-default history-length 1000
		use-dialog-box nil
		delete-by-moving-to-trash t
		create-lockfiles nil
		auto-save-default nil
		inhibit-startup-screen t
		ring-bell-function 'ignore)

  (setq	split-width-threshold 160
	split-height-threshold 120)

  ;;;; UTF-8
  (prefer-coding-system 'utf-8)

  ;;;; Remove Extra Ui
  (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
  (show-paren-mode 1)              ; Highlight parenthesis

  ;; TRAMP
  (require 'tramp)
  (setq tramp-default-method "ssh"
	shell-file-name "bash")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; ESHELL
  (require 'eshell)
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

  ;; DIRED
  (require 'dired)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-lh") ;; Hide hidden files by default

  ;; recentf
  (require 'recentf)
  (customize-set-value 'recentf-make-menu-items 150)
  (customize-set-value 'recentf-make-saved-items 150)
  (recentf-mode t)

  (desktop-save-mode 1)

  ;; Appearance
  (use-package modus-themes
    :config
    (setq modus-themes-bold-constructs t
	  modus-themes-italic-constructs t)
    (load-theme 'modus-operandi-tinted t)
    ;; (set-face-attribute 'fringe nil :background nil)
    )

  (when (display-graphic-p)
    (fringe-mode '(8 . 0)))

  ;; Basic Util Packages
  (use-package avy)
  (use-package ace-window
    :commands (aw-flip-window))
    :init
    (setq aw-background nil)
    (setq aw-dispatch-always t)
    (setq aw-frame-offset '(50 . 50))
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:foreground "#d00000" :height 1.0)))))

  (use-package crux)

  (use-package expand-region
    :bind (("C-=" . er/expand-region)))

  (use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (which-key-mode))

  (use-package delight
    :config
    (delight '((abbrev-mode " Abv" abbrev)
	       (smart-tab-mode " \\t" smart-tab)
	       (eldoc-mode nil "eldoc")
	       (rainbow-mode)
	       (overwrite-mode " Ov" t)
	       (emacs-lisp-mode "Elisp" :major)))))

(use-package custom-functions :straight nil  :ensure nil :no-require t
  :init
  (defun my/scroll-down (arg)
    "Move cursor down half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (next-line dist)))

  (defun my/scroll-up (arg)
    "Move cursor up half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (previous-line dist)))

  (defcustom list-of-dired-switches
    '("-lh" "-lah")
    "List of ls switches for dired to cycle among.")

  (defun cycle-dired-switches ()
    "Cycle through the list `list-of-dired-switches' of switches for ls"
    (interactive)
    (setq list-of-dired-switches
	  (append (cdr list-of-dired-switches)
		  (list (car list-of-dired-switches))))
    (setq dired-listing-switches (car list-of-dired-switches))
    (dired-sort-other (car list-of-dired-switches))))

;;; COMPLETION
(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (use-package vertico-posframe
    :when (window-system)
    :config
    (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center
	  vertico-count 20
	  vertico-posframe-min-height 1
	  vertico-posframe-min-width 90)
    (setq vertico-multiform-commands
      '((consult-line (:not posframe))
        (t posframe)))
    (vertico-posframe-mode 1))

  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex))
    ;; Allow tramp completion
    (completion-category-overrides '((file (styles basic partial-completion)))))

  (use-package marginalia
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
  (vertico-mode t)
  :config
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;; Extra Completion Functions
(use-package consult
  :after vertico
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (defun my/notegrep ()
    "Use interactive grepping to search my notes"
    (interactive)
    (consult-ripgrep org-directory)))

;;; Git
(use-package magit
  :after (project)
  :bind (("C-x g" . magit-status)
	 :map project-prefix-map
	 ("m" . project-magit))
  :commands (magit project-magit)
  :init
  (use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq-default git-gutter:update-interval 0)
  (custom-set-variables '(git-gutter:lighter " GG"))

  (use-package git-gutter-fringe
    :when (window-system)
    :config
    (require 'diff-mode)
    ;; (set-face-attribute 'git-gutter-fr:modified nil
    ;;			:background (face-attribute 'diff-changed :background)
    ;;			:foreground (face-attribute 'diff-changed :foreground))
    ;; (set-face-attribute 'git-gutter-fr:added nil
    ;;			:background (face-attribute 'diff-added :background)
    ;;			:foreground (face-attribute 'diff-added :foreground))
    ;; (set-face-attribute 'git-gutter-fr:deleted nil
    ;;			:background (face-attribute 'diff-removed :background)
    ;;			:foreground (face-attribute 'diff-removed :foreground))
    )
  :config
  (add-to-list 'project-switch-commands
	       '(project-magit "Magit" m))
  (defun project-magit  ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir)))))

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000)
  )

;;;; Code Completion
(use-package corfu
  :hook ((prog-mode) . corfu-mode)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(9999.9 . 0.3))
  (corfu-preview-current 'nil)
  (corfu-preselect 'directory)
  (corfu-on-exact-match 'quit)
  :init
  (setq tab-always-indent 'complete)

  (corfu-popupinfo-mode) ; Popup completion info
  (add-hook 'eshell-mode-hook
	    (lambda () (setq-local corfu-quit-at-boundary t
				   corfu-quit-no-match t
				   corfu-auto nil
				   corfu-auto-delay 1000)))
  (use-package corfu-terminal
    :when (not window-system)
    :config
    (corfu-terminal-mode +1)))

(use-package dabbrev  :straight nil
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package sqlite3)

(use-package fontaine
  :unless (not window-system)
  :config
  (setq fontaine-presets
	'((regular
	   :default-height 140)
	  (small
	   :default-height 130)
	  (large
	   :default-height 200)
	  (extra-large
	   :default-weight semilight
	   :default-height 210
	   :line-spacing 5
	   :bold-weight bold)
	  (t ; our shared fallback properties
	   :default-family "Fira Code"
	   :bold-weight semibold
	   :italic-slant italic)))
  (fontaine-set-preset 'regular))

;; Org Mode
(use-package org
  :bind (:map org-mode-map
	      ("C-c C-w" . nil))
  :config
  (setq org-pretty-entities t
	org-hide-emphasis-markers nil
	org-image-max-width 500)

  (use-package org-download
    :hook ((dired-mode) . org-download-enable)
    :custom
    (org-download-method 'directory)
    (org-download-image-dir "images")
    (org-download-heading-lvl nil)
    (org-download-timestamp "%Y%m%d-%H%M%S_")
    (org-download-screenshot-method "pngpaste %s")
    (org-download-annotate-function ((lambda (link))))
    :init
    (setq-default org-download-image-dir (concat org-directory "/img/")))

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

(use-package eglot  :straight nil
  :custom
  (eglot-workspace-configuration
   '((:gopls . (:linksInHover :json-false
			      :completeUnimported  t))
     (:grammarly . (:config . ((documentDialect . ("british")))))
     ))
  :config
  (use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :defer t  ; defer package loading
  :hook ((org-mode markdown-mode). (lambda ()
				     (require 'eglot-grammarly))))
  )



(use-package prog-mode :straight nil
  :hook ((prog-mode) . electric-pair-mode)
  :config
  (require 'cc-styles)
  (c-set-offset 'innamespace 0))

(use-package cmake-mode)
(use-package go-mode)
(use-package yaml-mode)
(use-package markdown-mode)

;; https://stackoverflow.com/a/77033292
(use-package tblui)
