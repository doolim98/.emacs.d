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
 compilation-scroll-output 'first-error
 ; compilation-scroll-output t
 split-window-keep-point t)

;; Tab Bar
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-close-button-show nil)
(setq tab-bar-new-button nil)
(tab-bar-mode 1)

;; Better Underline
(setq x-use-underline-position-properties t
	  x-underline-at-descent-line nil
	  underline-minimum-offset 1000)


;; Lowest font lock
(setq font-lock-maximum-decoration 2)

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

;; Remove wrapping chracter '\'
;; Notice that after the '\' there is a whitespace character but you
;; can use other characters.
(set-display-table-slot standard-display-table 'wrap ?\\)

(when (display-graphic-p)
  (fringe-mode '(8 . 1))
  (set-face-attribute 'fringe nil :background nil)
  ;; Disable line wrap indicators
  (setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil))
  (setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))
  (setq fontaine-presets
		'((regular		:default-height 140)
          (semi-small		:default-height 130)
		  (small		:default-height 120)
		  (semi-large	:default-height 150)
		  (large		:default-height 180)
		  (extra-large	:default-height 220)
		  (t
           :default-family "Iosevka"
             ;; :default-family "Fira Code"
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

;; (setq display-buffer-base-action
;;       ;; Copied from display-buffer-fallback-acion
;;       '((display-buffer--maybe-same-window display-buffer-reuse-window display-buffer--maybe-pop-up-frame-or-window display-buffer-in-previous-window display-buffer-use-some-window display-buffer-pop-up-frame)
;;         ;; (reusable-frames .t)
;;         (window-min-height . 0.4)
;;         ))

(setq display-buffer-alist '())

;; Side-window --  Use Bottom Right
(add-to-list 'display-buffer-alist `(,(rx (| "*async"
                                             ))
                                     (display-buffer-in-side-window)
                                     (side . bottom)
                                     (slot . 1) ;; -1 == L  0 == Mid 1 == R
                                     (window-height . 0.20)
                                     (window-parameters
                                      (no-delete-other-windows . nil))))
;; Side-Window --  Use Bottom Left
(add-to-list 'display-buffer-alist `(,(rx (| "*compilation*"
                                             ))
                                     (display-buffer-in-side-window)
                                     (side . bottom)
                                     (slot . -1) ;; -1 == L  0 == Mid 1 == R
                                     (window-height . 0.20)
                                     (window-parameters
                                      (no-delete-other-windows . nil))))


;; Mode line

(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))



(require 'my-modeline)
(run-with-timer 0 0.5 #'(lambda () (force-mode-line-update t)))


(setq-default mode-line-format
              `("%e" ; "%e" mode-line-front-space
                (:eval (let ((bufstr (format "%s%s " (if (buffer-modified-p) "*" " ") (buffer-name))))
                         (if (mode-line-window-selected-p)
                             (propertize bufstr 'face 'highlight)
                           (propertize bufstr 'face 'normal))))
                " "
                (:eval (my-mode-line-major-mode-indicator))
                " "
                (:eval (capitalize (string-replace "-mode" "" (symbol-name major-mode))))
                " "
                (:eval (when (mode-line-window-selected-p)
                         (list
                          '(:eval (mode-line-fill 'mode-line-active 45))
                          '(:eval (when (bound-and-true-p flymake-mode)
                                    (list
                                     (flymake--mode-line-counter :error t)
                                     (propertize "‼ " 'face 'shadow)
                                     (flymake--mode-line-counter :warning t)
                                     (propertize "! " 'face 'shadow)
                                     (flymake--mode-line-counter :note t))))
                          '(:eval (mode-line-fill 'mode-line-active 30))
                          '(:eval (propertize (format "%28s" (format-time-string "%m/%d %H:%M:%S"))
                                              'face 'bold)))))))

