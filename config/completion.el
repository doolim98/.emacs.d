(use-package vertico)
(use-package orderless :commands (orderless))
(use-package marginalia)
(use-package consult)
(use-package corfu)
(require 'dabbrev)
(require 'orderless)

(when (not window-system)
  (use-package corfu-terminal)
  (corfu-terminal-mode +1))

;; Common
;; ======
(setq tab-always-indent 'complete
	  c-tab-always-indent t)
;; The default completion-in-region-function
(setq completion-in-region-function #'consult-completion-in-region)
(setq-default dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))

;; Corfu
;; =====
(setq corfu-cycle t
	  corfu-auto t
	  corfu-auto-prefix 3
	  corfu-auto-delay 0.1
	  corfu-popupinfo-delay '(9999.9 . 0.3)
	  corfu-preview-current 'nil
	  corfu-preselect 'directory
	  corfu-on-exact-match 'insert)

;; Vertico
;; =======
(setq vertico-count 3
	  vertico-grid-min-columns 1
	  vertico-resize t)

;; Completion style
;; ================
(setq completion-styles '(orderless-fast basic partial-completion)
	  completion-category-overrides '((file (styles basic partial-completion flex))))

;; Faster orderless
(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; Hooks
;; =====
(defun corfu-mode-no-auto()
  (setq-local corfu-auto nil)
  (corfu-mode 1))

(defun corfu-mode-fast()
  (when (my/is-tramp)
	(setq-local corfu-auto-delay 0.5))
  (corfu-mode 1))

(add-hook 'prog-mode-hook 'corfu-mode-fast)
(add-hook 'eshell-mode-hook 'corfu-mode-no-auto)

;; Enable Modes
;; ============
(vertico-grid-mode 1)
(marginalia-mode 1)
(vertico-mode 1)
(vertico-reverse-mode 0)
