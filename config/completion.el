(require 'dabbrev)
(require 'orderless)

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
	  corfu-auto-delay 0.05
	  corfu-popupinfo-delay '(2.0 . 1.0)
	  corfu-preview-current 'nil
	  corfu-preselect 'directory
	  corfu-on-exact-match 'insert)

;; Vertico
;; =======
(setq vertico-count 7
	  vertico-grid-min-columns 1
	  vertico-resize t)

;; Completion style
;; ================
(setq completion-ignore-case  t)
(setq completion-styles '(orderless basic))
(setq completion-category-overrides
	  '((file (styles basic partial-completion flex))
		;; (command (styles orderless))
		;; (symbol (styles orderless))
		;; (variable (styles orderless))
		))

(setq orderless-component-separator "[. ]")

;; Faster orderless
(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
	   `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

(orderless-define-completion-style orderless+initialism
  (orderless-matching-styles '(orderless-initialism
							   orderless-literal
							   orderless-regexp)))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))


;; Hooks
;; =====
(defun corfu-mode-no-auto()
  (setq-local corfu-auto nil)
  (corfu-mode 1))

(defun corfu-mode-fast()
  (corfu-popupinfo-mode 1)
  (corfu-mode 1))

(add-hook 'prog-mode-hook 'corfu-mode-fast)
(add-hook 'eshell-mode-hook 'corfu-mode-no-auto)

;; Enable Modes
;; ============
;; (vertico-grid-mode 1)
(marginalia-mode 1)
(vertico-mode 1)
(vertico-reverse-mode 0)
(corfu-history-mode 1)
(global-corfu-mode 1)

(when (not window-system)
  (corfu-terminal-mode +1))
