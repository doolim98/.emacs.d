(setq-default
 delete-pair-blink-delay 0
 blink-cursor-delay 0.0
 blink-cursor-interval 0.2
 blink-cursor-blinks 9999
 cursor-type 'box
 use-dialog-box nil
 inhibit-startup-screen t
 ring-bell-function 'ignore
 split-width-threshold 160
 split-height-threshold 80
 use-short-answers t
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 even-window-heights nil)

(use-package modus-themes)
(setq modus-themes-bold-constructs t
	  modus-themes-italic-constructs t)
(load-theme 'modus-operandi-tinted t)
(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

(when (display-graphic-p)
  (fringe-mode '(8 . 0)))

(use-package fontaine
  :unless (not window-system)
  :config
  (setq fontaine-presets
		'((regular		:default-height 140)
		  (small		:default-height 120)
		  (semi-large	:default-height 160)
		  (large		:default-height 180)
		  (extra-large	:default-height 220)
		  (t :default-family "Iosevka"
			 :default-weight normal
			 :bold-weight semibold
			 :italic-slant italic)))
  (fontaine-set-preset
   (if (fontaine-restore-latest-preset)
	   fontaine-recovered-preset
	 'regular))
  (add-hook 'kill-emacs-hook 'fontaine-store-latest-preset))

(require 'org)
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
