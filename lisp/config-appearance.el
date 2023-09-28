(use-package modus-themes
  :config
  (setq modus-themes-bold-constructs t
		modus-themes-italic-constructs t)
  (load-theme 'modus-operandi-tinted t)
  ;; (set-face-attribute 'fringe nil :background nil)
  )

(when (display-graphic-p)
  (fringe-mode '(8 . 0)))

(use-package fontaine
  :unless (not window-system)
  :config
  (setq fontaine-presets
	'((regular
	   :default-height 140)
	  (small
	   :default-height 120)
	  (large
	   :default-height 200)
	  (extra-large
	   :default-weight semilight
	   :default-height 210
	   :line-spacing 5
	   :bold-weight bold)
	  (t ; our shared fallback properties
	   ;; :default-family "Fira Code"
	   :default-family "Iosevka"
	   :bold-weight semibold
	   :italic-slant italic)))
  (fontaine-set-preset 'regular))

(require 'org)
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; TODO Modeline configurationn

(provide 'config-appearance)
