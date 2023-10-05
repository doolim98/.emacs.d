;; Basic packages
;; ==============
(use-package general)
(use-package avy)
(use-package crux)
(use-package expand-region)
(use-package avy)
(use-package tblui)
(use-package sqlite3)
(use-package ace-window
  :commands (aw-flip-window)
  :init
  (setq aw-dispatch-always t))
(use-package which-key
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-idle-delay 1)
  (setq which-key-show-early-on-C-h t)
  (which-key-mode 1))

;; Theme
(use-package modus-themes)

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000))

;; Edit
;; ====
(use-package le-thesaurus)
(use-package cmake-mode)
(use-package go-mode)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package rainbow-mode)
(use-package csv-mode)

;; Completion
;; ==========
(use-package vertico)
(use-package orderless :commands (orderless))
(use-package marginalia)
(use-package consult)
(use-package corfu)
(use-package corfu-terminal)
(use-package embark)

;; Org
;; ===
(use-package org-download)
(use-package org-roam)
(use-package org)

(use-package polymode)
(use-package chatgpt :load-path "lisp/chatgpt")
(use-package grammarly)
(use-package flymake-grammarly)
