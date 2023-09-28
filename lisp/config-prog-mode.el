(use-package cmake-mode)
(use-package go-mode)
(use-package yaml-mode)
(use-package markdown-mode)

(require 'prog-mode)

(require 'electric)
(electric-pair-mode 1)

(require 'cc-styles)
(c-set-offset 'innamespace 0)

(require 'eglot)

(provide 'config-prog-mode)
