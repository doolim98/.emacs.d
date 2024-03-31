;; Basic packages
;; ==============
(use-package general)
(use-package avy)
(use-package crux)
(use-package expand-region)
(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m ?\;)))
(use-package tblui)
(use-package sqlite3)
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
(use-package ace-window
  :commands (aw-flip-window)
  :init
  (setq aw-dispatch-always nil))
(use-package which-key
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-idle-delay 2.0)
  (which-key-mode 1))
(use-package 0x0  :commands (0x0-dwim 0x0-upload-file))
(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))
(use-package loccur)
(use-package rg)
(use-package evil-numbers
  :config
  :bind (("C-c =" . #'evil-numbers/inc-at-pt)
         ("C-c -" . #'evil-numbers/dec-at-pt)))


;; Theme
(use-package modus-themes)
(use-package fontaine)
(use-package breadcrumb
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " > ")
  (breadcrumb-mode 1))

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000))

;; Edit
;; ====
(use-package le-thesaurus)
(use-package yasnippet
  :config
  (yas-global-mode 1))
(use-package yasnippet-snippets)
(require 'llvm-mode)
(use-package cmake-mode)
(use-package go-mode)
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package nasm-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))
(use-package markdown-mode)
(use-package rainbow-mode)
(use-package csv-mode)
(use-package denote)
(use-package gtags-mode)


;; Completion
;; ==========
(use-package vertico)
(use-package orderless :commands (orderless))
(use-package marginalia)
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq xref-show-xrefs-function #'xref--show-xref-buffer
        xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom)
  )
(use-package company :ensure t)
(use-package company-reftex
  :config
  (defun my/add-company-reftex-backend()
    (add-to-list 'company-backends 'company-reftex-citations)
    (add-to-list 'company-backends 'company-reftex-labels))
  (add-hook 'reftex-mode-hook #'my/add-company-reftex-backend))
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

;; Org
;; ===
(use-package org)
(use-package org-download)
;; (use-package org-roam)


(use-package polymode)
(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))
(use-package gptel
  :config
  (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY")))

(use-package tex
  :straight nil
  :ensure auctex)

(when (memq window-system '(mac ns x))
  (use-package pdf-tools
    :ensure t
    :config
    (custom-set-variables
     '(pdf-tools-handle-upgrades nil)) ; Use brew upgrade pdf-tools instead.
    (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
    (pdf-tools-install)
    (setq pdf-view-display-size 'fit-height)
    ;; (add-hook)
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
  )
