(defun my/font-lock-add-lambda ()
  (font-lock-add-keywords nil
						  `(("\\<lambda\\>"
							 (0 (progn
								  (compose-region (match-beginning 0) (match-end 0)
												  ,(make-char 'greek-iso8859-7 107))
								  nil))))))

(use-package fontaine :ensure t
  :if (display-graphic-p)
  :config
  (setq fontaine-presets
		`(
		  (size-regular		:default-height 140)
		  (size-semi-small	:default-height 130)
		  (size-small		:default-height 120)
		  (size-tiny		:default-height 110)
		  (size-semi-large	:default-height 150)
		  (size-large		:default-height 180)
		  (extra-large		:default-height 220)

		  ;; (font-Fira-Mono		:default-family "Fira Mono")
		  (font-FiraCode		:default-family "FiraCode Nerd Font Mono")
		  (font-Consolas		:default-family "Consolas")
		  (font-Iosevka
		   :default-family "Iosevka"
		   :fixed-pitch-family "Iosevka Slab"
		   )
		  (font-Arial			:default-family "Arial")
		  (font-IosevkaSlab			:default-family "Iosevka Slab")
		  (font-IosevkaTermSlab	:default-family "IosevkaTermSlab Nerd Font Mono")
		  (font-CaskaydiaMono	:default-family "CaskaydiaMono Nerd Font")
		  (t
           ;; :default-family "Iosevka Nerd Font"
             ;; :default-family "Fira Mono"
             ;; :default-family "Fira Code"
             ;; :default-family "CaskaydiaMono Nerd Font"
		     :default-weight regular

			 :variable-pitch-family "Arial"
			 :variable-pitch-height 1.0

			 :bold-weight semi-bold
			 :italic-slant italic

			 :fixed-pitch-family "IosevkaTermSlab Nerd Font Mono"
			 )))
  (set-frame-parameter (selected-frame) 'line-spacing 0.05)
  )

;; Nerd Icons 󰄛  
(use-package nerd-icons :ensure t
  :config
  (when (display-graphic-p)
	(unless (find-font (font-spec :name nerd-icons-font-family))
	  (nerd-icons-install-fonts t)))
  (setq-default nerd-icons-scale-factor 0.8
				nerd-icons-default-adjust 0.2
				nerd-icons-font-family "Symbols Nerd Font Mono"
				)
  (setq-default nerd-icons-color-icons t)
  (use-package nerd-icons-dired :ensure t
	:hook
	(dired-mode . nerd-icons-dired-mode)
	:config
	(setq-default nerd-icons-dired-v-adjust 0.0)
	)
  (use-package nerd-icons-corfu :after corfu :ensure t
	:config
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (use-package nerd-icons-completion :after marginalia)
	:config
	(nerd-icons-completion-mode 1)
	(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'my-fonts)
