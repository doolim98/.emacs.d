;; Based on https://github.com/Gavinok/emacs.d/blob/main/init.el
(straight-use-package 'org)

;; Load project related configurations
(load (concat user-emacs-directory "lisp/config-project.el"))

(use-package custom-functions :straight nil  :ensure nil :no-require t
  :after (emacs dired)
  :bind (("M-[" . previous-buffer)
	 ("M-]" . next-buffer)
	 ("C-d" . my/scroll-down)
         ("C-u" . my/scroll-up)
	 :map dired-mode-map
	 ("-" . dired-up-directory)
	 ("." . cycle-dired-switches))
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

(use-package emacs :straight nil :ensure nil :defer nil
  :bind (("C-c C-w"   . fixup-whitespace)
         ("M-1" . delete-other-windows)
         ("M-2" . split-window-below)
         ("M-3" . split-window-right)
	 )
  :config
  (setq-default delete-pair-blink-delay 0)

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

  ;; DIRED
  (require 'dired)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-listing-switches "-lh")

  ;; recentf
  (recentf-mode t)
  (customize-set-value 'recentf-make-menu-items 150)
  (customize-set-value 'recentf-make-saved-items 150)

  ;; Window apperance
  
  ;; Appearance
  (use-package modus-themes
    :config
    (load-theme 'modus-operandi t)
    (setq modus-themes-bold-constructs t
	  modus-themes-italic-constructs t)
    (set-face-attribute 'fringe nil :background nil))

  (when (display-graphic-p)
    ;; make the left fringe 4 pixels wide and the right disappear
    (fringe-mode 4))

  ;; Basic Util Packages
  (use-package ace-window
    :init
    (global-set-key (kbd "M-o") 'ace-window))

  (use-package crux
    :bind (("C-o" . crux-smart-open-line)
	   ("C-x C-u" . crux-upcase-region)
	   ("C-x C-l" . crux-downcase-region)
	   ("C-x M-c" . crux-capitalize-region)))

  (use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (which-key-mode)))

;;; COMPLETION
(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

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
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;; Extra Completion Functions
(use-package consult
  :after vertico
  :bind (("C-x b"       . consult-buffer)
         ("C-x C-k C-k" . consult-kmacro)
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
         ("C-x C-SPC"   . consult-global-mark)
         ("C-x M-:"     . consult-complex-command)
         ("C-c n"       . consult-org-agenda)
         ("C-c m"       . my/notegrep)
         :map help-map
         ("a" . consult-apropos)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (defun my/notegrep ()
    "Use interactive grepping to search my notes"
    (interactive)
    (consult-ripgrep org-directory))
  (recentf-mode t))

(use-package embark
  :ensure t
  :bind
  ;; pick some comfortable binding
  (("C-q" . embark-act)
   ("M-."                     . embark-act)
   ("C-<escape>"              . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d"                     . dragon-drop)
   ;; :map embark-defun-map
   ;; :map embark-general-map
   ;; :map embark-region-map
   )
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (defun search-in-source-graph (text))
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drop " file))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (:all embark consult)
  :demand t)

;;; Git
(use-package magit
  :bind (("C-x g" . magit-status)
         :map project-prefix-map
         ("m" . project-magit))
  :commands (magit project-magit)
  :config
  (add-to-list 'project-switch-commands
	       '(project-magit "Magit" m))
  (defun project-magit  ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir))))

;;; VTERM AND ESHELL
(use-package vterm
  :bind (("C-x t" . vterm))
  :commands vterm
  :custom (vterm-max-scrollback 10000)
  )

(use-package eshell :straight nil
  :bind ("C-x E" . eshell)
  :init
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  :config
  (setopt eshell-prompt-function
          (lambda ()
            (concat (abbreviate-file-name (eshell/pwd))
                    (if-let ((status eshell-last-command-status))
			(if (= status 0) "" (format " [%s]" status)))
                    (if (= (user-uid) 0) " # " "$"))))

  (require em-alias)
  (add-hook 'eshell-mode-hook
            (lambda ()
	      (eshell/alias "e" "find-file $1")
	      (eshell/alias "ee" "find-file-other-window $1")
	      (eshell/alias "v" "view-file $1")
	      (eshell/alias "o" "crux-open-with $1")))

  (require em-term)
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-options '("ghcup" "tui"))
  (add-to-list 'eshell-visual-commands '("htop" "top" "git" "log" "diff"
                                         "show" "less" "nix")))

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.3 . 0.1))
  (corfu-preview-current 'insert) 
  (corfu-preselect 'directory)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
	      ("M-SPC"      . corfu-insert-separator)
	      ("TAB"        . corfu-insert)
	      ([tab]        . corfu-insert)
	      ("<return>" . corfu-insert)
	      ("RET"        . corfu-insert)
	      ("ESC" . keyboard-quit)
	      )
  :init
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  (corfu-history-mode)

  (corfu-popupinfo-mode) ; Popup completion info
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
				   corfu-quit-no-match t
				   corfu-auto nil
				   corfu-auto-delay 1000)
	      ))
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
           :default-height 110)
          (large
           :default-height 200)
          (extra-large
           :default-weight semilight
           :default-height 210
           :line-spacing 5
           :bold-weight ultrabold)
          (t                        ; our shared fallback properties
           :default-family "Fira Code"
	   :bold-weight bold
           :italic-family nil
           :italic-slant italic
	   ;; :line-height 2.0
           ;; :line-spacing 0.0
	   )))
  (fontaine-set-preset 'regular))

;; Org Mode
(use-package org
  :config
  (setq org-pretty-entities t
	org-hide-emphasis-markers nil)

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
   '(org-level-2 ((t (:inherit outline-1 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-1 :height 1.0)))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Helvetica" :height 180
				 :weight light))))
   '(fixed-pitch ((t ( :family "Fira Code" :height 140))))))


(use-package eglot  :straight nil
  :bind (:map eglot-mode-map
	      ("C-c C-q" . eglot-code-action-quickfix))
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

(use-package go-mode)
(use-package yaml-mode)
(use-package electric-pair-mode
  :straight nil
  :hook ((prog-mode) . electric-pair-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 1)
  
  (use-package git-gutter-fringe
    :when (window-system)))



;; https://stackoverflow.com/a/77033292
(use-package tblui)
