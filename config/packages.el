;; Emacs Basic Configurations
;; ==========================
(setq straight-use-package-by-default nil) ; Don't use straight for basic configs
(use-package emacs :ensure nil
  :config
  (setq-default
   tab-width 4
   history-length 1000)
  ;; System
  (setq garbage-collection-messages nil
        gc-cons-threshold (* 10 1024 1024))

  ;; File managements
  (setq-default
   create-lockfiles nil
   auto-save-default nil
   make-backup-files nil
   auto-save-timeout 5
   revert-without-query t
   auto-revert-interval 1
   )
  ;; Window
  (setq-default
   scroll-step 1                        ; not sure :(
   scroll-conservatively 1              ; not sure too :(
   auto-window-vscroll t
   recenter-redisplay nil               ; Prevents annoying flashs on C-l in terminal emacs
   help-window-select t
   split-window-keep-point nil
   enable-recursive-minibuffers nil
   shell-command-dont-erase-buffer nil)
  )
(use-package dired :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "gls"))
  (setq dired-kill-when-opening-new-dired-buffer t
        dired-dwim-target t
        dired-listing-switches "-aBhl --group-directories-first"))
(use-package tramp :ensure nil
  :config
  (tramp-cleanup-all-connections)
  (setq enable-remote-dir-locals nil)
  (setq remote-file-name-inhibit-cache nil)
  (setq remote-file-name-inhibit-locks t)
  (setq tramp-default-method "sshx")
  (setq tramp-use-scp-direct-remote-copying t)
  (setq tramp-use-ssh-controlmaster-options t
        tramp-ssh-controlmaster-options
	    (concat
	     "-o ControlPath=tramp.%%C "
	     "-o ControlMaster=auto -o ControlPersist=yes"))
  ;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-verbose 1))

;; Essential Packages
;; ==================
(setq straight-use-package-by-default t) ; Use straight for 3rd-party packages
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package magit)
(use-package rg)

;; Basic Packages
;; ==============
(use-package general)
(use-package crux)
(use-package expand-region)
(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m ?\;)))
;(use-package sqlite3)

(use-package ace-window
  :commands (aw-flip-window)
  :init
  (setq aw-dispatch-always nil))

;; Theme
(use-package modus-themes)
(use-package fontaine)

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000))

;; Edit & Language
;; ===============
(use-package yasnippet
  :config
  (yas-global-mode 1))
(require 'llvm-mode)
(use-package ein)
(use-package cmake-mode)
(use-package go-mode)
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package nasm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))
(use-package markdown-mode)
(use-package rainbow-mode)
(use-package denote)
(use-package gtags-mode)

(use-package recentf :ensure nil
  :config
  (setq recentf-auto-cleanup 'never)
  (setq-default recentf-max-menu-items 150
              recentf-max-saved-items 150)
  (recentf-mode 1))

;; Completion
;; ==========
(use-package vertico
  :config
  (setq vertico-count 13
	  vertico-resize 'grow-only)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (vertico-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        orderless-component-separator "[. ]"))
(use-package marginalia
  :config
  (marginalia-mode 1))
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'xref--show-xref-buffer
        xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom)
  )
;; (use-package company :ensure t)

(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (setq corfu-auto t
        corfu-quit-no-match t))


;; (use-package cape
;;   ;; Available: cape-file cape-dabbrev cape-history cape-keyword
;;   ;; cape-tex cape-sgml cape-rfc1345 cape-abbrev cape-ispell
;;   ;; cape-dict cape-symbol cape-line
;;   :init
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev 90))

(use-package corfu-terminal
  :after corfu
  :init (corfu-terminal-mode))

(use-package embark-consult)
(use-package embark)
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
(use-package org)
(use-package org-download)
(use-package biblio)

(use-package tex :straight auctex)

(when (memq window-system '(mac ns x))
  (use-package pdf-tools
    :ensure t
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
    :ensure t
    :config
    ;; Make sure to set this variable before you enable `openwith-mode`.
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp
                  '("graffle" "docx" "doc" "hwp" "hwpx"))
                 "open"
                 '(file))))
    (openwith-mode t))
  )



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
