(setq-default
 delete-pair-blink-delay 0
 blink-cursor-delay 0.0
 blink-cursor-interval 0.2
 blink-cursor-blinks 9999
 cursor-type 'box
 use-dialog-box nil
 ;; inhibit-startup-screen t
 ring-bell-function 'ignore
 use-short-answers t
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 truncate-lines nil)

;; Tab Bar
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button nil)
(setq tab-bar-new-tab-choice "*scratch*")
(tab-bar-mode 1)


;; Theme
;; =====
(defun my/enable-modus-themes()
  (setq modus-themes-bold-constructs t
		modus-themes-italic-constructs t)
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (load-theme 'modus-operandi t))

;; (my/enable-modus-themes)

(when (display-graphic-p)
  (fringe-mode '(8 . 8)))

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
			 :bold-weight bold
			 :italic-slant italic)))
  (fontaine-set-preset
   (if (fontaine-restore-latest-preset)
	   fontaine-recovered-preset
	 'regular))
  (add-hook 'kill-emacs-hook 'fontaine-store-latest-preset))

(require 'org)
;; (custom-set-faces
;;  '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
;;  '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
;;  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
;;  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
;;  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; Color
;; =====
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Buffer Display Management
;; =========================
;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Choosing-Window.html

(setq even-window-heights 'width-only
	  split-width-threshold 100
	  split-height-threshold 100)
(setq switch-to-buffer-obey-display-actions t)
(setq display-buffer-alist
	  `(;; (,(rx (| "*dictionary" "*chatgpt"))
		;;  (display-buffer-reuse-window display-buffer-in-side-window)
		;;  (side . bottom)
		;;  (window-height . 0.3))
		;; (,(rx (| "*help" "*eldoc"))
		;;  (display-buffer-pop-up-window)
		;;  (window-height . 0.3))
	  ))
