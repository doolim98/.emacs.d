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
		  (size-18 :default-height 180)
		  (size-15 :default-height 150)
		  (size-14 :default-height 140)
		  (size-13 :default-height 130)
		  (size-12 :default-height 120)
		  (size-11 :default-height 110)

		  (font-Menlo
		   :default-family "Menlo"
		   :default-weight medium
		   :bold-weight bold)

		  ;; Does Monaco unsupport bold font?
		  (font-Monaco
		   :default-family "Monaco"
		   :default-weight medium
		   :bold-weight bold
		   :fixed-pitch-family "Monaco"
		   )

		  (font-FiraCode
		   :default-family "FiraCode Nerd Font Mono")

		  (font-Iosevka
		   :default-family "Iosevka"
		   :default-weight medium
		   :fixed-pitch-family "Iosevka Slab")
		  (t
		   :fixed-pitch-family "Iosevka"
		   :fixed-pitch-height 1.0
		   :bold-weight bold
		   :variable-pitch-family "Arial"
		   :variable-pitch-height 1.0
		   )))
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
