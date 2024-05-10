;; References
;; - https://www.mattduck.com/2023-08-28-extending-use-package-bind
;;
;; Emacs Basic Configurations
;; ==========================
(require 'my-common)
(setq straight-use-package-by-default nil
      use-package-always-ensure nil)

(use-package emacs
  :bind (("M-/" . #'comment-line)))
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; Meta Key Configuration
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))
(setq-default ring-bell-function 'ignore) ; Remove ring bell

(savehist-mode 1)
(setq-default history-length 1000)

(repeat-mode 1)
(setq set-mark-command-repeat-pop t)

;; Font lock optimization
(setq font-lock-maximum-decoration 2)

;; Editor
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(setq-default tab-width 4)
(electric-pair-mode 1)

;; System
(setq garbage-collection-messages nil
      gc-cons-threshold (* 10 1024 1024))

;; File managements
(setq-default create-lockfiles nil
              auto-save-default nil
              make-backup-files nil
              auto-save-timeout 5
              revert-without-query t
              auto-revert-interval 1)
(recentf-mode 1)
(setq-default recentf-auto-cleanup (* 60 60 24 7)
              recentf-max-menu-items 150
              recentf-max-saved-items 150)

;; Window
(setq-default scroll-step 1           ; not sure :(
              scroll-conservatively 1 ; not sure too :(
              auto-window-vscroll t
              recenter-redisplay nil ; Take away the annoying flashs on every C-l in terminal emacs
              help-window-select t
              split-window-keep-point nil)

(require 'my-window)
(use-package emacs
  :bind (("C-1" . my/select-window-1)
		 ("C-2" . my/select-window-2)
		 ("C-3" . my/select-window-3)
		 ("C-4" . my/select-window-4))
  )


(use-package hippie-expand :ensure nil :straight nil
  :bind (("M-'" . hippie-expand))
  :config)
(use-package dabbrev
  :config
  (setq-default
   dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")
   dabbrev-abbrev-skip-leading-regexp
   (rx (or "!" "@" "#" "$" "%" "^" "&" "*" "_" "-" "+" "=" "'" "/" "`" "'" "{" "}"))))

(use-package modus-themes :ensure t :straight t
  :config
  (modus-themes-load-theme 'modus-operandi))

;; https://emacs.stackexchange.com/questions/69074/how-to-make-buffer-order-in-tab-line-persistent
(use-package tab-line
  :bind (("M-w" . #'bury-buffer)
		 ("M-c" . #'kill-ring-save)
		 ("M-W" . #'unbury-buffer)
         ("M-[" . #'tab-line-switch-to-prev-tab)
         ("M-]" . #'tab-line-switch-to-next-tab))
  :init
  (global-tab-line-mode 1)
  (setq-default 
   ;; tab-line-separator (propertize (propertize "| " 'face 'shadow) 'face 'bold)
   ;; tab-line-format '(:eval )
   ;; tab-line-separator (propertize "🬓 " 'face '(:foreground "gray40"))
   tab-line-new-button-show nil
   tab-line-close-button-show nil
   tab-line-close-button ""))
(use-package dired
  :bind (:map dired-mode-map
              ("-" . 'dired-up-directory)
              ("." . 'my/cycle-dired-switches))
  :config
  ;; 'gls' breaks dried over tramp, when the remote doesn't have 'gls'
  ;; (when (string= system-type "darwin")
  ;;   (setq dired-use-ls-dired t
  ;;         insert-directory-program "gls"))
  ;; (setq dired-kill-when-opening-new-dired-buffer t
  ;;       dired-dwim-target t
  ;;       dired-listing-switches "-aBhl --group-directories-first")
  )
(use-package tramp
  :config
  (tramp-cleanup-all-connections)
  (setq enable-remote-dir-locals nil
        remote-file-name-inhibit-cache nil
        remote-file-name-inhibit-locks t
        tramp-default-method "sshx"
        tramp-use-scp-direct-remote-copying t
        tramp-use-ssh-controlmaster-options t
        tramp-ssh-controlmaster-options
        (concat
         "-o ControlPath=~/.ssh/.tramp.%%C "
         "-o ControlMaster=auto -o ControlPersist=yes"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-verbose 1))

;; Essential Packages
;; ==================
(setq straight-use-package-by-default t
      use-package-always-ensure t)
(use-package exec-path-from-shell
  :init
  (when (my/is-window-system)
    (exec-path-from-shell-initialize)))
(use-package magit)
(use-package rg)

(use-package ha-evil :straight nil :load-path "lisp/"
  :config
  (evil-mode 1))

;; Basic Packages
;; ==============
(use-package crux
  :bind (("C-a" . 'crux-move-beginning-of-line)
		 ("C-x x r" . 'crux-rename-buffer-and-file)))
(use-package expand-region
  :bind (("M-h" . er/expand-region)
		 ("C-x H" . er/contract-region))
  :custom (er/try-expand-list '(er/mark-inside-quotes er/mark-outside-quotes er/mark-inside-pairs er/mark-outside-pairs er/mark-comment er/mark-url er/mark-email er/mark-defun)))
(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m ?\;)))
(use-package ace-window
  :bind (("M-o" . #'ace-window))
  :commands (aw-flip-window)
  :init
  (setq aw-dispatch-always nil))
;; Theme
(use-package modus-themes)
(use-package fontaine
  :ensure nil
  :when (display-graphic-p)
  :ensure t
  :config
  (setq fontaine-presets
		'((regular		:default-height 140)
		  (semi-small	:default-height 130)
		  (small		:default-height 120)
		  (semi-large	:default-height 150)
		  (large		:default-height 180)
		  (extra-large	:default-height 220)
		  (t
           :default-family "Iosevka"
             ;; :default-family "Fira Code"
		     :default-weight normal
			 :bold-weight semibold
			 :italic-slant italic)))
  (fontaine-set-preset 'regular))

(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000))

;; Completion
;; ==========
(setq-default completion-ignore-case  t
              read-file-name-completion-ignore-case t
              read-buffer-completion-ignore-case t)
(use-package vertico
  :bind (:map vertico-map
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word))
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 13
	    vertico-resize 'grow-only)
  ;; (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  )
;; (require 'vertico)
;; (vertico-mode 1)

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        orderless-component-separator "[. ]")
  ;; Override completion style on files
  (setq completion-category-overrides
        '((file
           (styles basic partial-completion flex))
          (xref-location
           (styles substring))))
  ;; https://github.com/minad/corfu?tab=readme-ov-file#auto-completion
  ;; Fast custom orderless style
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         (cons 'orderless-literal-prefix word)))
  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp))))
(use-package marginalia
  :config
  (marginalia-mode 1))
(use-package consult
  :after vertico
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'xref--show-xref-buffer
        xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom)
  )
(use-package corfu
  :hook ((prog-mode . corfu-mode))
  :init
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)
  (corfu-popupinfo-mode 1)
  (setq corfu-min-width 50
		corfu-auto t
        corfu-cycle t
        corfu-quit-no-match 'separator)
  (defun minimal-corfu-mode()
    (setq-local corfu-auto nil
                corfu-popupinfo-mode 0
                completion-styles '(basic))))
(use-package corfu-terminal
  :after corfu
  :when (not (my/is-window-system))
  :config (corfu-terminal-mode 1))
(use-package embark-consult)
(use-package embark)

;; Edit & Language
;; ===============
(use-package eglot
  :bind (("C-x l" . eglot)
         :map eglot-mode-map
         ("C-." . eglot-code-action-quickfix)
         ("C-c ." . eglot-code-action-quickfix)
         ("C-c C-f" . eglot-format-buffer)
         ("C-c C-r" . eglot-rename)
         ("C-x l" . eglot-reconnect)))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)))
(require 'llvm-mode)
(use-package ein)
(use-package cmake-mode)
(use-package go-mode)
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package nasm-mode :mode ("\\.asm\\'" . nasm-mode))
(use-package markdown-mode)
(use-package gtags-mode)
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'consult-xref)
  (setq dumb-jump-project-denoters
        '(".dumbjump" ".projectile" ".git"))

  ;; (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-force-searcher nil)
  (setq dumb-jump-prefer-searcher 'git-grep)
  )


;; Writing & Latex
;; ===============
(use-package org :pin manual)
(use-package org-download)
(use-package biblio)
(use-package tex :straight auctex)

(use-package pdf-tools
  :when (my/is-window-system)
  :config
  (custom-set-variables
   '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (pdf-tools-install)
  (setq pdf-view-display-size 'fit-height
        pdf-view-resize-factor 1.5
        pdf-view-continuous t
        pdf-view-bounding-box-margin 0.2)
  ;; (add-hook)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
(use-package openwith
  :when (my/is-window-system)
  :config
  ;; Make sure to set this variable before you enable `openwith-mode`.
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("graffle" "docx" "doc" "hwp" "hwpx"))
               "open"
               '(file))))
  (openwith-mode t))


(use-package polymode)
(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :config
  (add-hook 'chatgpt-mode-hook #'(lambda()(setq-local comint-scroll-to-bottom-on-output t)))
  (exec-path-from-shell-copy-env "OPENAI_API_KEY")
  ;; See https://github.com/ahmetbersoz/chatgpt-prompts-for-academic-writing
  (setq chatgpt-code-query-map
	    '(
          ("lipsumX4" . "
You are my computer-engineering paper writer.
Increase my writing by 4 times in academic style.
Add notification that this is dumm text at first sentence.")

		  ("improve" . "
You are my english writing assistance.
Improve the coherence and cohesivity of my writing.
Do not use be verb if you can.
Also, do not change the latex syntax.")

          ("short" . "
You are my english writing assistance.
Improve the coherence and cohesivity of my writings in one sentence.
If you think the input is too long, you can write two or three sentences but each sentence should have single idea.
The result should contain all my sentences implicitly with appropriate words and adjectives.
However, don't be too flashy and always write clear sentences.
Also, do not change the latex syntax, keep latex commands such as `\cite{*,*,*}, \label{*}, \cref{*},\ref{*}`.")

          ("academic" . "
You are my computer-engineering paper writer.
Improve the coherence and cohesivity of my writing in academic style.
Also, do not change the latex syntax.")

          ("short" . "
You are my english writing assistance.
Make my sentences shorter and more concise by using appropriate adjectives and nouns.")

          ("naming" . "
You are my english writing assistance.
I want to find good name of the following description.
Suggest me 10 names")

          ("bug" . "There is a bug in the following, please help me fix it.")
		  ("doc" . "Please write the documentation for the following.")
		  ("refactor" . "Please refactor the following.")
		  ("suggest" . "Please make suggestions for the following.")))
  )

(use-package gptel
  :config
  (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY")))


(setq my/cloud-directory
	  (expand-file-name
	   (let ((path-1 "~/Library/Cloudstorage/Dropbox/")
			 (path-2 "~/Dropbox/"))
		 (if (file-exists-p path-1) path-1 path-2))))

(setq my/cloud-directory (expand-file-name "~/Dropbox/"))
(put 'list-timers 'disabled nil)
