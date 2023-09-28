(require 'general)

(general-define-key
 :keymaps 'override
 "C-4" 'ctl-x-4-prefix
 "C-c C-w"'fixup-whitespace
 "C-x C-k" 'kill-this-buffer
 "C-x b" 'consult-buffer
 "M-s" 'save-buffer
 "M-y" 'consult-yank-pop
 "M-o" 'ace-window
 "M-1" #'(lambda()(interactive)(execute-kbd-macro "\3571"))
 "M-2" #'(lambda()(interactive)(execute-kbd-macro "\3572"))
 "M-3" #'(lambda()(interactive)(execute-kbd-macro "\3573"))
 "M-4" #'(lambda()(interactive)(execute-kbd-macro "\3574"))
 "M-5" #'(lambda()(interactive)(execute-kbd-macro "\3575"))
 "M-[" 'previous-buffer
 "M-]" 'next-buffer
 "C-x t" 'vterm
 "C-x C-e" 'eshell
 ;; Cursor
 "C-j" 'avy-goto-word-1
 "C-o" 'crux-smart-open-line
 "C-a" 'crux-move-beginning-of-line
 "C-=" 'er/expand-region
 ;; Up/Down case
 "C-x C-u" 'crux-upcase-region
 "C-x C-l" 'crux-downcase-region
 "C-x M-c" 'crux-capitalize-region
 ;; Language Server 
 "C-x l" 'eglot)

(general-define-key
 :keymaps 'global
 "<escape>" 'keyboard-quit
 "C-v" 'my/scroll-down
 "M-v" 'my/scroll-up)

(general-unbind "C-x C-o")
(general-define-key
 :prefix "C-x C-o"
 "c" 'my/find-file-emacs-configs
 "o" 'my/find-file-org-directory)

(general-unbind "s-,")
(general-define-key
 :prefix "s-,"
 "s-," 'my/find-file-emacs-configs
 "," 'my/find-file-emacs-configs
 "f" 'fontaine-set-preset)

(general-define-key
 :keymaps 'isearch-mode-map
 "C-l" 'consult-line
 "C-o" 'isearch-occur)

(general-define-key
 :keymaps 'eglot-mode-map
 "C-c C-q" 'eglot-code-action-quickfix
 "C-c C-f" 'eglot-format-buffer)

(general-define-key
 :keymaps 'dired-mode-map
 "-" 'dired-up-directory
 "." 'my/cycle-dired-switches)


(general-define-key
 :prefix "M-g"
 "o" 'consult-outline
 "i" 'consult-imenu)




(provide 'config-keybindings)
