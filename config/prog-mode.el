(use-package cmake-mode)
(use-package go-mode)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package rainbow-mode)
(require 'prog-mode)
(require 'electric)
(require 'cc-styles)
(require 'eglot)

(setq eldoc-echo-area-use-multiline-p nil
	  flymake-no-changes-timeout 1)
(c-set-offset 'innamespace 0)
;; (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
(setq eglot-autoreconnect 0.5)


(electric-pair-mode 1)
