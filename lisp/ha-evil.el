;; ha-evil.el --- my evil configuration

(use-package general :ensure t)

(defun ha-evil/init()
  (setq evil-want-keybinding nil
		evil-want-C-u-scroll t
		evil-undo-system 'undo-redo))

(defun ha-evil/keymap-unset(keymap-list key-list)
  (dolist (km keymap-list)
	(dolist (k key-list)
	  (define-key km (kbd k) nil))))



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
		 ("SPC x" . #'execute-extended-command)
		 ("SPC e f" . #'eval-defun)
		 ("SPC e f" . #'eval-)
		 ("SPC h v" . #'describe-variable)
		 ("SPC h f" . #'describe-function)
		 ("SPC h F" . #'describe-face)
		 :map evil-normal-state-map
		 ("SPC" . nil)
		 ("SPC [" . #'previous-buffer)
		 ("SPC ]" . #'next-buffer)
		 ("SPC f" . #'find-file)
		 ("SPC F" . #'find-file-other-window)
		 ("SPC b" . #'switch-to-buffer)
		 ("SPC B" . #'switch-to-buffer)
		 ("SPC SPC" . #'switch-to-buffer)
		 ("M-." . nil)
		 )
  :config
  (evil-mode 1)
  (add-hook 'special-mode-hook #'evil-motion-state)
  (global-set-key (kbd "<escape>") (kbd "C-g"))
  (setq evil-motion-state-modes 
		(append evil-emacs-state-modes evil-motion-state-modes))
  (setq evil-emacs-state-modes nil)
  (ha-evil/keymap-unset
   (list evil-normal-state-map
		 evil-insert-state-map
		 evil-motion-state-map)
   (list "C-n" "C-p" "C-b" "C-f" "C-a" "C-e" "C-k"))
  (use-package evil-terminal-cursor-changer
	:when (not (display-graphic-p))
	)
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

(provide 'ha-evil)
