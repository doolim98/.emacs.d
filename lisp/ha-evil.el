;; ha-evil.el --- my evil configuration

(defun ha-evil/init()
  (setq evil-want-keybinding nil
		evil-want-C-u-scroll t
		evil-undo-system 'undo-redo))

(defun ha-evil/keymap-unset(keymap-list key-list)
  (dolist (km keymap-list)
	(dolist (k key-list)
	  (define-key km (kbd k) nil))))


;; (defvar-keymap ha-evil/leader-keymap :prefix)

(use-package evil
  :ensure t
  :init
  (ha-evil/init)
  :bind (("M-s" . #'save-buffer)
		 :repeat-map my/tab-line--repeat
		 ("]" . #'next-buffer)
		 ("[" . #'previous-buffer)
		 :map evil-motion-state-map
		 ("RET" . nil)
		 ("q" . nil)
		 ("TAB" . nil)
		 ("SPC" . nil)
		 ("SPC [" . #'previous-buffer)
		 ("SPC ]" . #'next-buffer)
		 :map evil-normal-state-map
		 ("SPC" . nil)
		 ("SPC [" . #'previous-buffer)
		 ("SPC ]" . #'next-buffer)
		 ("SPC f" . #'find-file)
		 ("SPC F" . #'find-file-other-window)
		 ("SPC b" . #'switch-to-buffer)
		 ("SPC B" . #'switch-to-buffer)
		 ("SPC h v" . #'switch-to-buffer)
		 ("SPC SPC" . #'switch-to-buffer)
		 ("M-." . nil)
		 )
  :config
  (evil-mode 0)
  (global-set-key (kbd "<escape>") (kbd "C-g"))
  (ha-evil/keymap-unset
   (list evil-normal-state-map
		 evil-insert-state-map
		 evil-motion-state-map)
   (list "C-n" "C-p" "C-b" "C-f" "C-a" "C-e" "C-k"))
  (use-package evil-collection
	:after evil
	:init
	)
  (use-package evil-terminal-cursor-changer
	:when (not (display-graphic-p))
	)
  (defface ha-evil/state-emacs-face
	'((t (:background "Green" :foreground "Blue")))
	"Evil Mode Emacs State Face")

  (defface ha-evil/state-insert-face
	'((t (:background "DodgerBlue1" :foreground "White")))
	"Evil Mode Insert State Face")

  (defface ha-evil/state-normal-face
	'((t (:background "Red" :foreground "White")))
	"Evil Mode Normal Stace Face")

  (defface ha-evil/state-visual-face
	'((t (:background "Purple" :foreground "White")))
	"Evil Mode Normal Stace Face")
  (use-package key-chord
	:init
	(key-chord-mode 1)
	(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
	))


(provide 'ha-evil)
