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
  (setq aw-dispatch-always t))
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

(use-package ibuffer
  :config
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("eglot" (predicate eglot-managed-p))
                 ("prog" (used-mode . prog-mode))
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))
                 )))))

(use-package hydra)

;; Theme
(use-package modus-themes)
(use-package fontaine)

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000))

;; Edit
;; ====
(use-package le-thesaurus)
(use-package yasnippet
  ;; :disabled t
  :config
  (yas-global-mode 1))
;; (use-package yasnippet-snippets :disabled t)
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
(use-package csv-mode)
(use-package denote)


;; Completion
;; ==========
(use-package vertico)
(use-package orderless :commands (orderless))
(use-package marginalia)
(use-package consult)
(use-package company :ensure t)

(use-package corfu
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (setq corfu-auto t
        corfu-quit-no-match t))
(use-package cape
  ;; Available: cape-file cape-dabbrev cape-history cape-keyword
  ;; cape-tex cape-sgml cape-rfc1345 cape-abbrev cape-ispell
  ;; cape-dict cape-symbol cape-line
  :after company
  ;; :disabled t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev 90)
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'cape-keyword nil t)))
  :config
  (require 'company)
  (cl-loop for backend in '(company-cmake company-etags company-gtags)
           do (add-hook 'completion-at-point-functions
                        (cape-company-to-capf backend)))
  ;; (add-hook 'cmake-mode-hook (lambda() (add-hook 'completion-at-point-functions )))
  )

;; (use-package nerd-icons-corfu
;;   :after corfu
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-terminal
  :after corfu
  :init (corfu-terminal-mode))

(use-package embark-consult)
(use-package embark)

;; Org
;; ===
(use-package org)
(use-package org-download)
;; (use-package org-roam)


(use-package polymode)
(use-package chatgpt
  :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
  :config
  (add-hook 'chatgpt-mode-hook #'(lambda()(setq-local comint-scroll-to-bottom-on-output t))))
(use-package gptel
  :config
  (setq gptel-api-key (exec-path-from-shell-getenv "OPENAI_API_KEY")))

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
    (setq pdf-view-display-size 'fit-height)
    ;; (add-hook)
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
  )


