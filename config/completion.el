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
	  corfu-popupinfo-delay '(1.0 . 0.5)
	  corfu-preview-current 'nil
	  corfu-preselect 'directory
	  corfu-on-exact-match 'insert
	  corfu-count 5)

;; Vertico
;; =======
(setq vertico-count 7
	  vertico-grid-min-columns 1
	  vertico-resize 'grow-only)

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
;; Use . as separator(for corfu)
(setq orderless-component-separator "[. ]")

;; Hooks
;; =====
(defun corfu-mode-no-auto()
  (setq-local corfu-auto nil)
  (corfu-mode 1))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil
                ;; corfu-popupinfo-delay nil
				)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
(add-hook 'eshell-mode-hook 'corfu-mode-no-auto)

;; Enable Modes
;; ============
;; (vertico-grid-mode 1)
(marginalia-mode 1)
(vertico-mode 1)
(vertico-reverse-mode 0)
(corfu-history-mode 1)
(global-corfu-mode 1)
(corfu-popupinfo-mode 1)

(when (not window-system)
  (corfu-terminal-mode +1))
