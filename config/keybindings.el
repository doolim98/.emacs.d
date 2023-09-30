(require 'general)

;; Mac OSX
;; =======
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
		mac-option-modifier 'super))

;; Basic
;; =====
(general-define-key
 :keymaps 'override
 "C-4" 'ctl-x-4-prefix
 "C-\\" 'ctl-x-4-prefix ;; ="C-4" in terminal
 "C-c C-w"'fixup-whitespace
 "C-x C-k" 'kill-this-buffer
 "C-x b" 'consult-buffer
 "C-x C-r" 'crux-rename-buffer-and-file
 "M-y" 'consult-yank-pop
 "C-o" 'crux-smart-open-line
 "C-a" 'crux-move-beginning-of-line)

;; Move & Window
;; =============
(general-define-key
 :keymaps 'override
 ;; Cursor Move
 "C-j" 'avy-goto-word-1
 "C-=" 'er/expand-region
 "C--" 'er/contract-region
  ;; Window Manipulation
 "M-z" 'my/toggle-window-size
 "M-o" 'ace-window
 "M-O" 'aw-flip-window
 "M-0" 'delete-window
 ;; NOTE: this keybindings imitate `ace-window' operations using "M-O" keybindings
 ;; "\3571" -> "M-o 1" "\3171" -> "M-O 1"
 "M-1" #'(lambda()(interactive)(execute-kbd-macro "\3571"))
 "M-2" #'(lambda()(interactive)
		   (when (> 2 (length (window-list)))
			 (split-window-right))
		   (execute-kbd-macro "\3572"))
 "M-3" #'(lambda()(interactive)
		   (when (> 3 (length (window-list)))
			 (split-window-right)
			 (balance-windows))
		   (execute-kbd-macro "\3573"))
 "M-4" #'(lambda()(interactive)(execute-kbd-macro "\3574"))
 "M-5" #'(lambda()(interactive)(execute-kbd-macro "\3575")))

;; Shortcuts
;; =========
(general-define-key
 :keymaps 'override
 "C-x g" 'magit
 "C-x C-t" 'vterm
 "C-x M-m" 'compile
 "C-x C-m" 'recompile
 "C-x C-e" 'eshell
"s-<backspace>" 'backward-kill-word
 ;; Up/Down case
 "C-x C-u" 'crux-upcase-region
 "C-x C-l" 'crux-downcase-region
 "C-x M-c" 'crux-capitalize-region)
(general-define-key
 "C-x l" 'eglot)

;; Settings
;; ========
(general-define-key
 :prefix "C-x ,"
 "," 'my/find-file-emacs-configs
 "r" 'my/reload-user-init-file
 "f" 'fontaine-set-preset
 "t" 'modus-themes-toggle)

;; Search & Navigate
;; =================
(general-define-key
 :prefix "M-s"
 "l" 'consult-line
 "M-l" 'consult-line-multi
 "M-g" 'consult-git-grep
 "g" 'consult-ripgrep)
(general-define-key
 :prefix "M-g"
 "o" 'consult-outline
 "i" 'consult-imenu
 "f" 'consult-flymake)

;; Eglot
;; =====
(general-define-key
 :keymaps 'eglot-mode-map
 "C-c C-q" 'eglot-code-action-quickfix
 "C-c C-f" 'eglot-format-buffer
 "C-c C-r" 'eglot-rename
 "C-x l" 'eglot-reconnect)

;; Corfu
;; =====
(general-define-key
 :keymaps 'corfu-map
 "C-h" 'corfu-popupinfo-toggle)

;; Dired
;; =====
(general-define-key
 :keymaps 'dired-mode-map
 "-" 'dired-up-directory
 "." 'my/cycle-dired-switches)


;; Project
;; =======
(general-define-key
 :keymaps 'project-prefix-map
 "m" 'project-magit)
