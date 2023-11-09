(require 'prog-mode)
(require 'electric)
(require 'cc-styles)
(require 'eglot)
(require 'grammarly)
(require 'flymake-grammarly)

(setq-default indent-tabs-mode nil)

(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit
      eldoc-echo-area-display-truncation-message nil
	  flymake-no-changes-timeout 0.5)
(c-set-offset 'innamespace 0)
(setq eglot-ignored-server-capabilities
      '(:documentHighlight :documentOnTypeFormattingProvider :inlayHint))

(electric-pair-mode 1)

(defun my/prog-mode-hook ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)
(add-hook 'text-mode-hook 'flyspell-mode)

;; Auto Revert
;; ===========
(defun my/immediate-auto-revert-mode()
  (setq-local auto-revert-interval 1
			  auto-revert-remote-files t)
  (auto-revert-mode 1))

(add-hook 'image-mode-hook 'my/immediate-auto-revert-mode)

