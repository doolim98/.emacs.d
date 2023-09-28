;; Minimal pacakges
;; 1. No default hook
;; 2. No default keybindings
;; Basic packages
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
  (setq which-key-show-early-on-C-h t)
  (which-key-mode))

;;; VTERM AND ESHELL
(use-package vterm
  :commands vterm
  :custom (vterm-max-scrollback 10000))



(provide 'config-minimal-packages)
