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


(use-package magit)

;; Theme
(use-package modus-themes)
(use-package fontaine)
(use-package spacious-padding
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 0
         :header-line-width 2
         :mode-line-width 3
         :tab-width 2
         :right-divider-width 20
         :scroll-bar-width 0
         :left-fringe-width 8
         :right-fringe-width 8))
  (setq spacious-padding-subtle-mode-line
        nil
  ;; '(:mode-line-inactive error)
  )
  (spacious-padding-mode 1))

;; (use-package breadcrumb
;;   :config
;;   (setq breadcrumb-project-max-length 0.5)
;;   (setq breadcrumb-project-crumb-separator "/")
;;   (setq breadcrumb-imenu-max-length 1.0)
;;   (setq breadcrumb-imenu-crumb-separator " > ")
;;   (breadcrumb-mode 1))

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
(use-package denote)
(use-package gtags-mode)

(use-package csv-mode
  :config
  (setq csv-align-min-width 10)
  (setq csv-align-padding 2)
  (defun my/csv-mode-hook()
    (progn
      (setq-local buffer-invisibility-spec nil)
      (csv-guess-set-separator)
      (csv-align-mode 0)))

  (defun my/csv-mode-untabify ()
    "Replace whitespace, including tabs, between characters with a single space."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(.\\)[[:space:]]+\\(.\\)" nil t)
        (replace-match (format "\\1%s\\2" (car csv-separators)) nil nil))))

  (add-hook 'csv-mode-hook 'my/csv-mode-hook))

(use-package recentf :ensure nil
  :config
  (setq recentf-auto-cleanup 'never)
  (setq-default recentf-max-menu-items 150
              recentf-max-saved-items 150)
  (recentf-mode 1))

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

;; Org
;; ===
(use-package org)
(use-package org-download)
;; (use-package org-roam)


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


