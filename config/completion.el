(require 'vertico-directory)
(require 'dabbrev)
(require 'orderless)

;; Common
;; ======
(setq tab-always-indent 'complete
	  c-tab-always-indent t)
(setq completion-ignore-case  t)
(setq completion-styles '(basic orderless))
(setq completion-category-overrides '((file (styles basic partial-completion flex))))
(setq orderless-component-separator "[. ]") ; Use . as separator(for corfu)
(setq completion-in-region-function #'consult-completion-in-region)
(setq-default dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")
			  dabbrev-abbrev-skip-leading-regexp
			  (rx (| "!" "@" "#" "$" "%" "^" "&" "*" "_" "-" "+" "=" "'" "/" "'" "`" "'" "{" "}")))

;; Company
;; =======
(require 'company)
(setq company-semantic-insert-arguments t)
(setq company-insertion-on-trigger nil)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-maximum-width 40)
(setq company-tooltip-limit 7)
(setq company-backends
      '(company-bbdb company-semantic company-cmake company-capf company-clang company-files
                     (company-gtags company-etags company-keywords)
                     company-oddmuse))

(defun my/company-org-mode-hook()
  (setq-local company-backends '(company-capf company-dabbrev-code company-ispell)))

(global-company-mode 0)
;; (company-tng-mode 1)

(add-hook 'org-mode-hook 'my/company-org-mode-hook)

(add-hook 'prog-mode-hook (lambda ()
			   (add-function :before-until (local 'electric-pair-inhibit-predicate)
					 (lambda (c) (eq c ?<)))))

(add-hook 'org-mode-hook (lambda ()
			   (add-function :before-until (local 'electric-pair-inhibit-predicate)
					 (lambda (c) (eq c ?<)))))


;; Vertico
;; =======
(setq vertico-count 10
	  vertico-grid-min-columns 1
	  vertico-resize 'grow-only)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(marginalia-mode 1)
(vertico-mode 1)

