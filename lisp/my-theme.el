;;; my-theme.el --- My Theme

;;; Commentary:
;;
(require 'cl-lib)
(require 'cl-macs)
(require 'nerd-icons)
(require 'my-fonts)

(defvar my-theme-init-hook '() "Hook that run after init my theme")

(use-package ef-themes)
(use-package modus-themes
  :config
  (setq modus-themes-mixed-fonts t
		modus-themes-bold-constructs t)
   
  (defun my-modus-themes-hook()
	(set-face-attribute 'fringe nil :background (face-background 'default)))
  (add-hook 'modus-themes-after-load-theme-hook 'my-modus-themes-hook))

(defun my-theme-init()
  ;; Mode-line
  ;; Remove Backgrounds
  (dolist (face '(fringe line-number line-number-current-line))
	(set-face-background face nil))

  ;; Dim Fringe
  (dolist (face '(fringe))
	(set-face-foreground face (my-face-background 'default 6)))

  ;; Frame Parameters
  (modify-all-frames-parameters
   '((internal-border-width . 1)
	 (scroll-bar-width . 11)
	 (no-special-glyphs . t)
	 (vertical-scroll-bars . nil)))

  ;; Config Windov Divider
  (setq-default window-divider-default-right-width 1
                window-divider-default-bottom-width 0
                window-divider-default-places t)

  (let ((div-color (my-face-foreground 'default 8)))
	(dolist (face
			 '(internal-border window-divider window-divider-first-pixel window-divider-last-pixel))
	  (set-face-attribute face nil :background div-color :foreground div-color)))

  (set-face-attribute 'tab-line nil :weight 'bold :height 1.0 :box nil)
  (defun my-tab-line-format()
	(face-remap-add-relative 'tab-line `(:background ,(face-background 'header-line)))
	(concat
	 (propertize (concat " " (nerd-icons-icon-for-buffer) " " (buffer-name) " ")
				 'face `(:background ,(my-face-background 'tab-line 1)))
	 (file-remote-p default-directory)
	 ""
	 (propertize (concat " "(abbreviate-file-name default-directory))
				 'face `(:foreground ,(my-face-foreground 'default 7)))
	 ))

  ;; (setq-default header-line-format '(:eval (my-header-line-format)))
  (setq-default header-line-format nil)
  (setq-default tab-line-format '(:eval (my-tab-line-format)))
  (setq-default tab-line-format nil)

  (my-tab-init)

  ;; Load Modes
  (window-divider-mode 1)
  (global-tab-line-mode 0)
  )

(defun my-tab-init()
  (defface my-tab-separator nil "My tab separator face")
  (defface my-tab-nerd-icon nil "My tab nerd icon face")
  (set-face-attribute 'my-tab-separator nil :inherit 'tab-line :weight 'normal
					  :foreground (my-face-background 'tab-line 2) :background nil)
  ;; Tab Bar
  (setq-default tab-bar-separator " "
				tab-bar-new-button nil
				tab-bar-close-button-show nil)
  (set-face-attribute 'tab-bar-tab-inactive nil 
					  :foreground (my-face-foreground 'default 2)
					  :background (my-face-background 'tab-line 2)
					  )

  ;; Tab Line
  ;; Attributes
  (set-face-attribute 'tab-line-tab-inactive nil :box nil
					  :foreground (my-face-foreground 'default 5)
					  :background nil :inherit 'tab-line)
  (set-face-attribute 'tab-line-tab-current nil :box nil)
  (set-face-attribute 'tab-line-tab nil :inherit 'tab-line-tab-current)

  (setq-default tab-line-exclude-modes '(completion-list-mode minibuffer-mode))

  (setq-default tab-line-separator (propertize "|" 'face '(:inherit '(my-tab-separator)))
				;; tab-line-format '(:eval (tab-line-format))
				tab-line-tab-name-function 'my-tab-line-tab-name-function
				tab-line-tab-name-format-function 'my-tab-line-tab-name-format))


(defun my-tab-line-tab-name-function (buffer &optional _buffers)
  (concat "" (tab-line-tab-name-buffer buffer) ""))

(defun my-tab-line-tab-name-format(tab tabs)
  (let* ((pos-visible-tab (cl-position (current-buffer) tabs))
		 (pos-current-tab (cl-position tab tabs))
		 (buffer-p (bufferp tab))
		 (selected-p (if buffer-p
						 (eq tab (window-buffer))
					   (cdr (assq 'selected tab))))
		 (tab-name-format (tab-line-tab-name-format-default tab tabs))
		 (tab-face (get-text-property 0 'face tab-name-format))
		 (is-visible-tab (eq pos-visible-tab pos-current-tab))
		 (nerd-icon (with-current-buffer tab (nerd-icons-icon-for-buffer)))
		 (nerd-icon-prop (get-text-property 0 'face nerd-icon)))
	(concat (propertize " " 'face tab-face)
			(propertize nerd-icon 'face `(:family ,nerd-icons-font-family
										  :inherit ,(if selected-p
														'tab-line-tab-current
													  'tab-line-tab-inactive)))
										  ;; :inherit ,(if (plist-member tab-face :inherit)
										  ;; 				;; (plist-get tab-face :inherit)
										  ;; 				'tab-line-tab-inactive
										  ;; 			  'tab-line-tab-inactive))
			(propertize " " 'face tab-face)
			tab-name-format
			(propertize " " 'face tab-face)
			)))

(defun my-get-bg-color()
  (face-attribute 'default :background))
(defun my-get-fg-color()
  (face-attribute 'default :foreground))

(defvar my-color-scale 0.05 "My color scale light parameter")

(cl-defun my-color-alt (color factor &optional (scale my-color-scale))
  (let* ((rgb (color-name-to-rgb color))
		 (rgb (if rgb rgb '(0 0 0)))
		 (hsl (apply 'color-rgb-to-hsl rgb))
		 (delta-light (* (if (> (nth 2 hsl) 0.5) -1 +1) (* scale factor))))
	(apply 'color-rgb-to-hex
		   (apply 'color-hsl-to-rgb
				  (cl-mapcar '+ hsl `(0 0 ,delta-light))))))

(cl-defun my-color-bg (factor &optional (scale my-color-scale))
  (my-color-alt (face-attribute 'default :background) factor scale))

(cl-defun my-color-fg (factor &optional (scale my-color-scale))
  (my-color-alt (face-attribute 'default :foreground) factor scale))

(cl-defun my-face-background (face factor &optional (scale my-color-scale))
  (my-color-alt (face-attribute face :background) factor scale))

(cl-defun my-face-foreground (face factor &optional (scale my-color-scale))
  (my-color-alt (face-attribute face :foreground) factor scale))


(cl-defun my-vline(&optional (color my-color-div))
  (let ((my-vline-image-string  "#define image_width  1
#define image_height 1
static unsigned char image_bits[] = {0x00};"))
  (propertize " " 'display `(image :type xbm :data ,vline-image-string :margin (0 . 0))
			  'font-lock-face `(:background ,my-color-div :foreground ,my-color-div))))

(defun my-use-evil-insert-state-cursor-type()
  (setq-local cursor-type evil-insert-state-cursor))

(provide 'my-theme)

;;; my-theme.el ends here
