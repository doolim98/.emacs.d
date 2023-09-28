(use-package cmake-mode)
(use-package go-mode)
(use-package yaml-mode)
(use-package markdown-mode)

(require 'prog-mode)
(add-hook 'prog-mode 'electric-pair-mode)

(require 'cc-styles)
(c-set-offset 'innamespace 0)

(require 'eglot)

(provide 'config-prog-mode)
