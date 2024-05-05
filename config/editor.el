(require 'prog-mode)
(require 'electric)
(require 'cc-styles)
(require 'eglot)

(setq-default indent-tabs-mode nil)

(setq eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message nil
	  flymake-no-changes-timeout 1)
(c-set-offset 'innamespace 0)
(setq eglot-ignored-server-capabilities
      '(:documentHighlight :documentOnTypeFormattingProvider :inlayHint))

(setq-default eglot-workspace-configuration
    '((:pyright .
        ((useLibraryCodeForTypes . t)))))

(electric-pair-mode 1)

(defun my/prog-mode-hook ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'my/prog-mode-hook)
(add-hook 'text-mode-hook 'flyspell-mode)
