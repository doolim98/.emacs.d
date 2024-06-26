;; Add lisp/ into the load-path
(let ((default-directory (concat my-emacs-directory "./lisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;;; Initialize
(when window-system
  (select-frame-set-input-focus (selected-frame))
  (add-hook 'window-size-change-functions
			#'frame-hide-title-bar-when-maximized)
  (unless (eq 'maximized (frame-parameter (selected-frame) 'fullscreen))
	(toggle-frame-maximized)
	))
(load-file custom-file)
(server-start)

;; (use-package auto-package-update)

;;; Keybindings
(setq-default mac-wheel-button-is-mouse-2 nil)
(global-unset-key (kbd "<mouse-2>"))
(use-package region-bindings-mode :config (region-bindings-mode-enable))
(use-package general)
(setq-default mac-option-modifier 'meta ; 'super
			  mac-command-modifier 'super ; 'meta
			  mac-control-modifier 'control)

(repeat-mode 1)
(setq-default set-mark-command-repeat-pop t)

(defvar-keymap my-quote-map :doc "My quote keymap")
(defvar-keymap my-f-map :doc "My f keymap")
(defvar-keymap my-b-map :doc "My b keymap")
(defvar-keymap my-ctl-z-map :doc "My ctl-z keymap")
(defvar-keymap my-setting-map :doc "My settings keymap")
(defvar-keymap my-toggle-map :doc "My toggle keymap")

;;; Super Key
(general-def
  "C-s-f" #'toggle-frame-maximized)

(general-def
  "ESC ESC" "C-g"
  "s-o" #'crux-open-with
  "s-<backspace>" "M-<backspace>"
  "S-SPC" #'mark-sexp
  "C-j" #'crux-top-join-line
  "C-4" ctl-x-4-map
  "M-o" 'other-window
  "M-[" 'previous-buffer
  "M-]" 'next-buffer
  "M-O" 'window-swap-states
  "M-'" my-quote-map
  "C-t" my-toggle-map
  "C-x ," my-setting-map
  "C-x c" 'restart-emacs)

(general-def region-bindings-mode-map
  "i" #'string-rectangle
  "n" #'rectangle-number-lines
  "w" #'copy-region-as-kill
  "k" #'kill-region
  "q" #'query-replace
  "Q" #'query-replace-regexp
  "e" (kbd "C-c C-e")
  "ESC" nil								; Do not bind `ESC'!
  )

(defun my-display-other-with-keymap (keymap)
  (cond ((window-in-direction 'below) (windmove-display-down))
		((window-in-direction 'left) (windmove-display-left))
		((window-in-direction 'right) (windmove-display-right))
		(t))
  (set-transient-map keymap))

;;; Customize Built-in keymaps
;;;; `ctl-x-map'
(general-def ctl-x-map
  ;; Other window command
  "C-o" #'(lambda()(interactive) (my-display-other-with-keymap ctl-x-map))
  "f" my-f-map
  "b" #'consult-buffer
  "C-r" #'crux-rename-buffer-and-file
  "C-b" #'buffer-menu
  "C-k" #'kill-this-buffer)
;;;; `transient-map'
(general-def transient-map
  "<escape>" 'transient-quit-one)

;;;; `help-map'
(general-def help-map
  "C-h" nil
  "h" nil
  "d" #'devdocs-lookup
  "D" #'apropos-documentation
  "F" #'describe-face
  "K" #'describe-keymap)

;;;; `evil-window-map'
(general-def evil-window-map
  "u" #'winner-undo
  "C-r" #'winner-redo)

;;;; `search-map'
(general-def search-map
  "r" #'rg-project
  "l" #'consult-line
  "C-r" #'rg)

;;; My Custom Keymaps
(general-def my-toggle-map
  "f" #'flymake-mode
  ;; "l" #'(lambda()(interactive)
  ;; 		  (if (bound-and-true-p eglot--managed-mode)
  ;; 			  (eglot--managed-mode-off)
  ;; 			(progn
  ;; 			  (eglot--managed-mode)
  ;; 			  ;; HACK: change buffer
  ;; 			  (insert "@")
  ;; 			  (delete-backward-char 1)
  ;; 			  )))
  )
(general-def my-f-map
  "r" #'consult-recent-file
  "o" #'(lambda()(interactive)(let ((default-directory (expand-file-name "./" org-directory)))
							(call-interactively 'find-file))))

(general-def my-quote-map
  "s" #'scratch-buffer
  "m" #'(lambda()(interactive)(switch-to-buffer "*Messages*"))
  "i" #'(lambda()(interactive)(find-file (expand-file-name "init.el" my-emacs-directory)))

  "," #'(lambda()(interactive)(let ((default-directory (expand-file-name "./" my-emacs-directory)))
						   (call-interactively 'find-file)))
  "r" #'(lambda()(interactive)(load-file user-init-file))
  )

(general-def my-setting-map
  "t" #'consult-theme
  "," #'(lambda()(interactive)(let ((default-directory (expand-file-name "./" my-emacs-directory)))
						   (call-interactively 'find-file)))
  "r" #'(lambda()(interactive)(load-file user-init-file)))

;;; Emacs Defaults
(require 'tramp)
(require 'dired)
(require 'tab-line)
(setq-default inhibit-startup-screen t
			  ring-bell-function 'ignore ; Remove ring bell
			  make-pointer-invisible t)
(setq-default garbage-collection-messages nil
			  gc-cons-threshold (* 10 1024 1024))
(setq-default tooltip-delay 0.2)
(setq-default

 recentf-auto-cleanup (* 60 60 24 7)
 recentf-max-menu-items 150
 recentf-max-saved-items 150
 find-file-visit-truename nil

 create-lockfiles nil
 auto-save-default nil
 auto-save-timeout 5
 make-backup-files nil
 revert-without-query t
 auto-revert-interval 5
 auto-save-visited-interval 1
 history-length 1000

 ;; find-file-visit-truename t

 register-preview-delay 0.05

 desktop-save t
 desktop-restore-frames nil
 desktop-dirname user-emacs-directory
 destkop-path (list user-emacs-directory)

 ;; Window
 display-buffer-base-action '((
							   display-buffer--maybe-same-window
							   display-buffer-reuse-window
							   display-buffer--maybe-pop-up-frame-or-window
							   display-buffer-in-previous-window
							   display-buffer-use-some-window
							   display-buffer-pop-up-frame))
 display-buffer-base-action '(nil)
 split-width-threshold 160
 split-height-threshold 80

 auto-window-vscroll t
 recenter-redisplay nil ; Take away the annoying flashs on every C-l in terminal emacs
 help-window-select t
 split-window-keep-point t
 enable-recursive-minibuffers t

 ;; Editor
 tab-width 4
 parse-sexp-ignore-comments nil

 ;; Compilation
 compilation-scroll-output t
 compilation-ask-about-save nil

 ;; Font lock optimization
 font-lock-maximum-decoration 2

 ;; Bookmark
 bookmark-sort-flag 'last-modified)

(setq-default tramp-verbose 1					; Tramp
			  enable-remote-dir-locals t
			  remote-file-name-inhibit-cache nil
			  remote-file-name-inhibit-locks t
			  remote-file-name-inhibit-auto-save-visited t
			  tramp-default-method "sshx"
			  tramp-use-scp-direct-remote-copying t
			  tramp-use-ssh-controlmaster-options t
			  tramp-ssh-controlmaster-options "-o ControlPath=~/.ssh/.tramp.%%C -o ControlMaster=auto -o ControlPersist=yes")
(setq-default tab-bar-close-button-show nil)

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;;;; Modes
(desktop-save-mode 1)
(auto-save-visited-mode 1)
(auto-revert-mode 0)
(savehist-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;;; Appearance
(modify-all-frames-parameters
 '((inhibit-double-buffering . nil) ; Not sure
   (use-frame-synchronization . t)	; Prevent display tearing
   (ns-transparent-titlebar . t)	; No effects
   (internal-border-width . 10)		; Add margins at to the frame borders
   ))


(defvar my-hourly-timer nil)
(defun my-hourly-timer-hook '())
(unless my-hourly-timer
  (setq my-hourly-timer
		(run-with-timer 0 (* 60 60)
						#'(lambda()(run-hooks 'my-hourly-timer-hook)))))


(require 'help)
(general-def "C-`" #'window-toggle-side-windows)

(require 'my-fonts)
(setq-default line-spacing 0.0)
(fontaine-set-preset 'font-Monaco)		; Use Monaco!
(fontaine-set-preset 'size-12)
;; Hangul font
;; 가나|다라|
;; ABCD|EFGH|
(setq face-font-rescale-alist '(("D2Coding" . 1.2)))
(set-fontset-font t 'hangul (font-spec :name "D2Coding"))

(general-def my-setting-map "f" #'fontaine-set-preset)

(require 'my-theme)
(modus-themes-select 'modus-vivendi)
(add-hook 'my-after-enable-theme-hook 'my-remove-fringe-background)

;;;; Cursor
(setq-default blink-cursor-delay 0.2
			  blink-cursor-interval 0.1
			  blink-cursor-blinks 100
			  cursor-in-non-selected-windows t)

(use-package outline-minor-faces
  :after outline
  :config (remove-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode))

;;; Project Managements
(require 'project)
(require 'my-project)
(setq-default project-vc-merge-submodules nil
			  project-vc-extra-root-markers '(".git" ".dir-locals.el")
			  vc-handled-backends '(Git))
(use-package project :ensure nil
  :bind-keymap ("C-z" . project-prefix-map)
  :bind (:map project-prefix-map
			  ("C-c" . my/project-compile))
  :config
  )

(setq-default project-compilation-buffer-name-function #'my/project-compilation-buffer-name)

(cl-defun my/compilation-finish--revert-project-buffers (compilation-buffer message)
  (when (project-current)
	(my/project-revert-buffers))
  t)

(add-hook 'compilation-finish-functions 'my/compilation-finish--revert-project-buffers)

(cl-defun my/project-revert-buffers (&optional (project (project-current)))
  (dolist (buf (project-buffers project))
	(with-current-buffer buf
	  (when (and buffer-file-name
				 (file-exists-p buffer-file-name)
				 (not (buffer-modified-p)))
		(revert-buffer t t t)))))

(cl-defun my-add-project-local-variable()
  (interactive)
  (let ((default-directory (project-root (project-current))))
	(call-interactively 'add-dir-local-variable)))

;;; Compile
(defun my/compilation-finish--display-on-error (buffer outstr)
  (unless (string-match "finished" outstr)
    (switch-to-buffer-other-window buffer))
  t)

(add-hook 'compilation-finish-functions 'my/compilation-finish--display-on-error)
 
(defadvice compilation-start
  (around inhibit-display
      (command &optional mode name-function highlight-regexp))
  (if (not (string-match "^\\(find\\|grep\\)" command))
      (cl-letf ((display-buffer   #'ignore)
                (set-window-point #'ignoreco)
                (goto-char        #'ignore))
        (save-window-excursion
          ad-do-it))
    ad-do-it))

(ad-activate 'compilation-start)

;;; Tiny Utility Packages
(use-package saveplace :ensure nil
  :init (save-place-mode 1))

(use-package crux)
(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m ?\;)))

(use-package openwith
  :if (display-graphic-p)
  :config
  ;; Make sure to set this variable before you enable `openwith-mode`.
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("graffle" "docx" "doc" "hwp" "hwpx"))
               "open"
               '(file))))
  (openwith-mode t))

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(require 'dabbrev)
(setq-default
 dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")
 dabbrev-abbrev-skip-leading-regexp
 (rx (or "!" "@" "#" "$" "%" "^" "&" "*" "_" "-" "+" "=" "'" "/" "`" "'" "{" "}")))

(require 'dictionary)
(setq dictionary-server "dict.org"
      dictionary-default-popup-strategy "lev" ; read doc string
      dictionary-create-buttons nil
      dictionary-use-single-buffer t)

;; Dired
(require 'dired)
(require 'dired-x)
(require 'ls-lisp)
(general-def dired-mode-map
  "." #'dired-omit-mode
  "-" #'dired-up-directory)
(setq-default dired-listing-switches "-alh") ; Show human readable size
(setq-default dired-ls-sorting-switches "StU")
(setq-default dired-dwim-target t)

;; Use `ls-lisp'
(setq-default ls-lisp-use-insert-directory-program nil)
(setq-default ls-lisp-dirs-first t)
(setq-default ls-lisp-use-localized-time-format t)
(setq-default ls-lisp-format-time-list
			  '("%m-%d %H:%M"
				"    %Y-%m"))


(use-package dired-subtree :ensure t
  :bind (:map dired-mode-map
			  ("TAB" . #'dired-subtree-toggle)))

;; Use `dired-omit-mode'
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq-default dired-omit-files (rx bol (or (+ ?.))))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  )

(use-package magit :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-log-margin-show-committer-date t))

(use-package transient :ensure t
  :config
  (setq-default transient-display-buffer-action
				'(display-buffer-in-direction
				  ;; (side . bottom)
				  (direction . below)
				  (dedicated . t)
				  (inhibit-same-window . t)
				  (window-parameters (no-other-window . t)
									 (mode-line-format . none))))
  )

(use-package rg :ensure t)

(use-package vterm :ensure t
  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init
  :config
  )

;;; Completion
(setq-default completion-ignore-case  t
              read-file-name-completion-ignore-case t
              read-buffer-completion-ignore-case t)

(use-package vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (require 'vertico-directory)
  (vertico-mode 1)
  (setq vertico-count 10
	    vertico-resize 'grow-only))

(use-package orderless
  :config
  (setq-default completion-styles '(
							;; basic
							;; partial-completion
							;; initials
							orderless
							)
				orderless-component-separator "[. ]")

  ;; Override completion style on files
  (setq completion-category-overrides
        '((file (styles basic partial-completion orderless))
          (xref-location (styles substring)))))

(use-package marginalia
  :config
  (marginalia-mode 1)
  (setq-default marginalia-align 'left)
  (setq-default marginalia-field-width 300))


(use-package consult :after vertico
  :config
  ;; (general-defs minibuffer-mode-map
  ;; 	"C-s" #'consult-history
  ;; 	"C-s" #'consult-isearch-forward
  ;; 	"C-s" #'isearch-forward
  ;; 	"C-r" #'isearch-backward
  ;; 	)
  (general-defs
   "M-g i" #'consult-imenu
   "M-g o" #'consult-outline
   "M-g I" #'consult-imenu-multi
   "M-s g" #'consult-grep
   "M-s l" #'consult-line
   "M-s L" #'consult-line-multi
   ))

(use-package embark
  :bind
  (("C-." . embark-act-noquit)
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))
  :config

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-quit-after-action t)
  (setq embark-mixed-indicator-delay 99999)

  (setq embark-verbose-indicator-display-action '(display-buffer-at-bottom))

  (defun embark-act-noquit ()
	"Run action but don't quit the minibuffer afterwards."
	(interactive)
	(let ((embark-quit-after-action nil))
      (embark-act))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult :ensure nil
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :hook ((prog-mode . corfu-mode)
		 (tex-mode . corfu-mode)
		 (ielm-mode . corfu-mode)
		 (sly-mrepl . corfu-mode)
		 (org-mode . corfu-mode))
  :bind (:map corfu-map
			  ;; DO NOT USE `corfu-complete' for selection of candidates
			  ;; Use `corfu-insert' which is compatible with yasnippet
			  ("TAB" . corfu-insert)
			  ("<escape>" . corfu-quit)
			  ("<tab>" . corfu-insert)
			  ("RET" . nil))
  :config
  ;; (general-defs :keymaps 'corfu-map)
  ;; Variables
  (setq-default
   corfu-preselect 'valid
   corfu-count 10
   corfu-popupinfo-max-height 20
   corfu-min-width 22
   corfu-auto t
   corfu-auto-delay 0.1
   corfu-popupinfo-hide t
   corfu-popupinfo-delay 0.5
   corfu-cycle t
   corfu-quit-no-match 'separator
   tab-always-indent 'complete
   completion-cycle-threshold 3
   )

  ;; Modes
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (defun minimal-corfu-mode()
	"My minimal corfu mode for some shell"
	(interactive)
	(setq-local corfu-auto nil
				completion-styles '(basic))
	(corfu-mode 1))

  (defun corfu-enable-always-in-minibuffer ()
	"Enable Corfu in the minibuffer if Vertico/Mct are not active."
	(unless (or (bound-and-true-p mct--active)
				(bound-and-true-p vertico--input)
				(eq (current-local-map) read-passwd-map))
	  ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
	  (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
				  corfu-popupinfo-delay 99999)
	  (corfu-mode 1)))
  )

(use-package cape
  :config
  ;; (setq cape-dict-file )
  )

(use-package citar
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (setq citar-bibliography (list (expand-file-name "refer.bib" org-directory))))
 
(use-package corfu-terminal :after corfu
  :if (not (display-graphic-p))
  :config (corfu-terminal-mode 1))

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ("C-TAB" . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

;;; IDE
(use-package flymake :ensure nil
  :bind (:map flymake-mode-map
			  ("C-M-n" . flymake-goto-next-error)
			  ("C-M-p" . flymake-goto-prev-error))
  :config
  )
(use-package eldoc :ensure nil
  :config
  (add-to-list 'display-buffer-alist `(,(rx "*eldoc")
									   (display-buffer-pop-up-window)
									   ;; (side . bottom)
									   ;; (dedicated . t)
									   (inhibit-same-window . t)
									   ;; (window-parameters
									   ;; 	(no-other-window . t)
									   ;; 	(mode-line-format . none))
									   ))
  (setq-default eldoc-echo-area-prefer-doc-buffer t
				eldoc-documentation-strategy 'eldoc-documentation-enthusiast ; 'eldoc-doc
				;; eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly
				;; eldoc-documentation-strategy 'eldoc-documentation-compose
				eldoc-echo-area-use-multiline-p t)
  )
(use-package ediff :ensure nil
  :init
  (custom-set-variables
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   '(ediff-diff-options "-w")
   '(ediff-split-window-function 'split-window-horizontally))
  )

(use-package eglot
  :bind (("C-x x l" . eglot)
         :map eglot-mode-map
         ("C-c q" . eglot-code-action-quickfix)
         ("C-x TAB" . eglot-format-buffer)
         ("C-c r" . nil)
         ("C-x x l" . eglot-reconnect)
		 )
  :config
  (defvar-local my-eglot-disable-flymake nil)
  ;; (defun my-eglot-managed-hook-function()
  ;; 	(when (bound-and-true-p my-eglot-disable-flymake)
  ;; 	  (message "eglot-managed-hook: disable flymake")
  ;; 	  (flymake-mode 0)))
  ;; (add-hook 'eglot-managed-mode-hook 'my-eglot-managed-hook-function)
  ;; (add-hook 'eglot--managed-mode-hook 'my-eglot-managed-hook-function)

  ;; (setq-default eglot-prefer-plaintext nil)
  )

(use-package devdocs
  :bind (:map help-map
			  ("d" . #'devdocs-lookup)))

(use-package yasnippet
  :bind (:map yas-minor-mode-map
			  ("C-M-c C-y" . yas-expand))
  :hook ((prog-mode . yas-minor-mode))
  :init
  )

(use-package gtags-mode)
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'consult-xref)
  (setq dumb-jump-project-denoters
		'(".dumbjump" ".projectile" ".git"))
  ;; (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-force-searcher nil)
  (setq dumb-jump-prefer-searcher 'git-grep))
(use-package outline :ensure nil
  :hook ((prog-mode . outline-minor-mode)
		 (LaTeX-mode . outline-minor-mode))
  :config
  (setq outline-minor-mode-use-buttons nil))
;;; Languages
(use-package sly
  :config
(setq inferior-lisp-program "sbcl"))
(use-package elisp-mode :ensure nil
 :init (add-hook 'emacs-lisp-mode-hook #'my/font-lock-add-lambda)
 )
(use-package llvm-mode :ensure nil)
(use-package cmake-mode)
(use-package go-mode)
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package nasm-mode :mode ("\\.asm\\'" . nasm-mode))
(use-package markdown-mode
  :init
  (setq markdown-code-block-braces t))

;;; PDF
(use-package pdf-tools
  :if (display-graphic-p)
  :config
  ;; Disable cursor in pdf-view
  (defun disable-cursor-in-pdf-view()
	;; HACK: This will hide cursor even with blink-mode
	(setq-local evil-normal-state-cursor (list 'bar nil))
	(evil-refresh-cursor)
	)
  (remove-hook 'pdf-view-mode-hook 'disable-cursor-in-pdf-view)

  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install)
  (setq pdf-view-display-size 'fit-height
        pdf-view-resize-factor 1.5
        pdf-view-continuous t
        pdf-view-bounding-box-margin 0.2)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;;; Latex
(setq-default tex-default-mode 'tex-mode)
;; (use-package tex :ensure t)
;; (use-package tex :ensure auctex)


(require 'pdf-tools)
(defun my/find-TeX-master()
  (let* ((master-name "main.tex")
         (master-dir (locate-dominating-file "./" master-name)))
    (if master-dir
        (file-name-concat master-dir master-name)
      nil)))

(cl-defun my/latex-save-and-compile (&optional (command "latexmk"))
  (interactive)
  (let* ((caller-buffer (current-buffer))
		 (default-directory (project-root (project-current t))))
    (save-some-buffers t)
	(let ((compilation-finish-functions
		   '((lambda(buffer msg)
			   (dolist (buf (buffer-list))
				 (with-current-buffer buf
				   (when (and (string-match "\\.pdf\\'" (buffer-name))
							  (file-exists-p (buffer-file-name)))
					 (revert-buffer t t t))))))))
	  (project-compile command))))

;;;; tex-mode
(cl-defun my/tex-display-output (&optional (output-base-name "main.pdf")
										   (output-dirs '("./" "./build" "./output" "./out")))
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
		 (output-file (cl-find-if #'file-exists-p (mapcar #'(lambda(dir)(expand-file-name output-base-name dir)) output-dirs))))
	(if output-file
		(find-file-other-window output-file)
	  (message "Can't find output file!"))))

(defun my/tex-mode-setup()
  (setq eglot-workspace-configuration '(:digestif (:config (:data_dirs ("./" "./build")
																		:fuzzy-cite nil
																		:fuzzy-ref nil))))
  (local-set-key (kbd "C-c C-v") #'my/tex-display-output)
  (setq-local compile-command "latexmk")
  (eglot-ensure))

(add-hook 'tex-mode-hook #'my/tex-mode-setup)

;; Do not sue AucTex
(defun my/LaTeX-mode-setup()
  (setq TeX-output-dir "./build")
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server nil)
  (setq TeX-master (let ((master-name "main.tex"))
						   (expand-file-name master-name (locate-dominating-file "./" master-name))))

  (setq eglot-workspace-configuration '((digestif . (:config (:data_dirs ("./" "./build"))))))

  ;; Enforce to use `digestif'
  ;; (setq-local eglot-server-programs (list '((latex-mode) "digestif")))
  (eglot-ensure)

  ;; Disable some modes
  (electric-indent-local-mode -1)

  ;; Default compile-command
  (setq-local compile-command "latexmk")
  )
;; (add-hook 'LaTeX-mode-hook #'my/LaTeX-mode-setup)

;;; Writing
;;;; Ispell & Dictionary
(require 'ispell)
(general-def "s-;" #'ispell-continue)
(global-set-key (kbd "C-'") #'ispell-word)
(setq-default ispell-program-name "aspell"
			  ispell-silently-savep t
			  ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
			  ispell-personal-dictionary (expand-file-name "aspell.pws" my-emacs-directory))


(require 'flyspell)
(general-unbind flyspell-mode-map
  "C-;" "C-." "C-'")

(use-package org
  :config
  (general-def "s-a" #'org-agenda)
  (general-def org-mode-map
	"C-j" #'crux-top-join-line
	"C-c @" #'org-cite-insert
	)
  :config

  (setq-default org-directory "~/Dropbox/org"
				org-enforce-todo-dependencies nil
				org-agenda-files (list org-directory)
				org-agenda-include-diary t
				org-todo-keywords '((sequence "TODO" "PROG" "DONE"))
				org-todo-keyword-faces
				'(("PROG" . "blue"))
				)
  ;; Calander Window
  (add-to-list 'display-buffer-alist `(,(rx "*Calendar*")
									   (display-buffer-in-side-window)
									   (side . bottom)
									   (inhibit-same-window . t)
									   ))
  
  (defun my-org-mode-hook-function()
	(setq-local completion-ignore-case t) ; nil-> treat +BEGIN_SRC and +begin_src differently
	(setq-local truncate-lines nil)
	;; (setq-local my-eglot-disable-flymake t)
	(setq-local cape-dabbrev-check-other-buffers nil)
	(visual-line-mode 1)

	(setq-local completion-styles '(flex))
	(add-hook 'completion-at-point-functions #'cape-dabbrev)
	(add-hook 'completion-at-point-functions #'cape-file)
	(add-hook 'completion-at-point-functions #'cape-dict)

	(setq-local right-margin-width 10
				left-margin-width 10)
	;; (eglot-ensure)
	(org-indent-mode 1)
	;; (flyspell-mode 0)
	(auto-revert-mode 1)
	)

  (add-hook 'org-mode-hook #'my-org-mode-hook-function)

  (require 'ox-latex)
  ;; --shell-escape is added to support minted
  (setq org-latex-pdf-process '("latexmk -f -pdf -%latex --shell-escape -interaction=nonstopmode -output-directory=%o %f"))
  (add-to-list 'org-latex-packages-alist '("" "minted" nil))
  (setq org-latex-src-block-backend 'minted)

  (require 'ox-md)
  (setq org-export-with-tags nil)
  (add-to-list 'org-export-backends 'md)

  (require 'oc-biblatex)
  (add-to-list 'org-cite-biblatex-styles '("simple" nil "cite" nil nil))
  )

(use-package org-ref
  :config
  )
(use-package org-bullets :after org
  :hook ((org-mode . org-bullets-mode))
  :config
  (setq-default org-bullets-bullet-list '("*" "*" "*" "*" "*" "*"))
  )

(use-package ox-gfm)
;; (use-package org-download)
(use-package biblio)
(use-package flymake-grammarly :ensure t)

(use-package grammarly :ensure t
  :config
  (require 'eglot)
  ;; eglot support
  ;; Copied from https://github.com/emacs-grammarly/eglot-grammarly/blob/master/eglot-grammarly.el
  (defclass eglot-grammarly-server (eglot-lsp-server) ()
	:documentation "A custom class for grammarly langserver.")

  (cl-defmethod eglot-initialization-options ((server eglot-grammarly-server))
	"Passes through required grammarly initialization options"
    (list :clientId "client_BaDkMgx4X19X9UxxYRCXZo"))

  (add-to-list 'eglot-server-programs
               '((eglot-grammarly-mode org-mode markdown-mode) . (eglot-grammarly-server "grammarly-languageserver" "--stdio")))
  )


(use-package gptel
  :bind (("C-c G" . gptel)
		 ("C-c g" . gptel-menu)
		 :map region-bindings-mode-map
		 ("g" . gptel-menu))
  :config
  (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY"))
  (setq-default gptel-default-mode 'org-mode)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))


(require 'eww)
;; Install chromium
;; (unless (file-executable-p "chromium")
;;   (when (eq window-system 'mac)
;; 	(async-shell-command "brew install chromium --no-quarantine"))
;;   )

(setq eww-retrieve-command nil)
      ;; '("chromium" "--headless" "--dump-dom"))


;;; ETC

(winner-mode 1)

(setq dired-jump-map nil)
(fset 'yes-or-no-p 'y-or-n-p)			; don't ask to spell out "yes"


(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
      '(self-insert-command
        forward-char
        backward-char
        previous-line
        next-line)))


(garbage-collect)
