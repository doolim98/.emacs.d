;; Cursor
(setq-default
 delete-pair-blink-delay 0
 blink-cursor-delay 0.0
 blink-cursor-interval 0.2
 blink-cursor-blinks 9999)

(setq-default
 use-dialog-box nil
 ring-bell-function 'ignore
 use-short-answers t
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 shell-command-prompt-show-cwd t
 compilation-scroll-output t)

;; Tab Bar
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button nil)
(tab-bar-mode 1)

;; Better Underline
(setq x-use-underline-position-properties t
	  x-underline-at-descent-line nil
	  underline-minimum-offset 1000)

;; Theme
;; =====
(setq org-fontify-quote-and-verse-blocks t
	  org-fontify-whole-heading-line t)

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-variable-pitch-ui nil
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t
      modus-themes-prompts '(bold)
      modus-themes-completions
      '((matches . (extrabold))
        (selection . (semibold text-also)))

	  modus-themes-org-blocks nil
      modus-themes-headings
      '((1 . (extrabold 1.0))
        (2 . (semibold 1.0))
		(3 . (regular 1.0))
		(4 . (regular 1.0))
		(5 . (regular 1.0))
        (agenda-date . (1.3))
        (agenda-structure . (variable-pitch light 1.8))
        (t . (1.0))))

(setq modus-themes-common-palette-overrides
      '(;;(fg-heading-3 fg-main)
        ;;(fg-heading-4 fg-main)
        ;;(fg-heading-5 fg-main)
		))

(defun my/modus-themes-hook ()
  (custom-set-faces
   '(bold ((t :weight semibold)))))

(add-hook 'modus-themes-after-load-theme-hook #'my/modus-themes-hook)

;; Load the theme of your choice:
(load-theme 'modus-operandi :no-confirm)

(setq modus-themes-to-toggle '(modus-operandi modus-vivendi))

(when (display-graphic-p)
  (fringe-mode '(8 . 0))
  (setq fontaine-presets
		'((regular		:default-height 140)
		  (small		:default-height 120)
		  (semi-large	:default-height 150)
		  (large		:default-height 180)
		  (extra-large	:default-height 220)
		  (t
           ;; :default-family "Iosevka"
             :default-family "Fira Code"
		     :default-weight normal
			 :bold-weight semibold
			 :italic-slant italic)))
  (fontaine-set-preset
   (if (fontaine-restore-latest-preset)
	   fontaine-recovered-preset
	 'regular))
  (add-hook 'fontaine-set-preset-hook 'fontaine-store-latest-preset)
  )

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
	  split-width-threshold 160
	  split-height-threshold 100)
(setq switch-to-buffer-obey-display-actions nil)
(defun my/switch-to-buffer-list (buffer alist)
  (select-window  (display-buffer-use-some-window buffer alist)))
(setq display-buffer-alist
	  `(
        ;; (,(rx (| "*dictionary" "*chatgpt"))
		;;  (display-buffer-reuse-window display-buffer-in-side-window)
		;;  (side . bottom)
		;;  (window-height . 0.3))
		(,(rx (| "*compil"
                 "*chatgpt"
                 "*help"
                 ;; "*eldoc"
                 "*xxxx"))
		 ;;(display-buffer-reuse-window display-buffer-pop-up-window)
         (display-buffer--maybe-same-window display-buffer-below-selected)
         ;;(display-buffer-reuse-window display-buffer-in-side-window)
		 (window-height . 0.3))
		))


;; Mode line
;; =========
(with-eval-after-load 'subr-x
  (setq-default
   mode-line-buffer-identification
   '(:eval
     (format-mode-line
      (propertized-buffer-identification
       (or
        (when-let* ((buffer-file-truename buffer-file-truename)
                    (prj-parent-truename (file-name-directory
                                          (directory-file-name
                                           (expand-file-name
                                            (file-truename (project-root (project-current))))))))
          (concat
           (file-relative-name
            (file-name-directory buffer-file-truename) prj-parent-truename)
           (file-name-nondirectory buffer-file-truename)))
        "%b"))))))
