(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex))
    ;; Allow tramp completion
    (completion-category-overrides '((file (styles basic partial-completion)))))

  (use-package marginalia
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
  (vertico-mode t)
  :config
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult
  :after vertico
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (defun my/notegrep ()
    "Use interactive grepping to search my notes"
    (interactive)
    (consult-ripgrep org-directory)))
(provide 'config-completion)

(use-package corfu
  :hook ((prog-mode) . corfu-mode)
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(9999.9 . 0.3))
  (corfu-preview-current 'nil)
  (corfu-preselect 'directory)
  (corfu-on-exact-match 'quit)
  :init
  (setq tab-always-indent 'complete)

  (corfu-popupinfo-mode) ; Popup completion info
  (add-hook 'eshell-mode-hook
	    (lambda () (setq-local corfu-quit-at-boundary t
				   corfu-quit-no-match t
				   corfu-auto nil
				   corfu-auto-delay 1000)))
  (use-package corfu-terminal
    :when (not window-system)
    :config
    (corfu-terminal-mode +1)))

