(require 'general)

;; Mac OSX
;; =======
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super))

;; Basic
;; =====
(general-define-key
 "C-9" 'kmacro-start-macro-or-insert-counter
 "C-0" 'kmacro-end-or-call-macro
 "C-)" 'kmacro-set-counter
 "C-4" 'ctl-x-4-prefix
 "C-\\" 'ctl-x-4-prefix ;; ="C-4" in terminal
 "C-x C-k" 'kill-this-buffer
 "C-x b" 'consult-buffer
 "C-x x r" 'crux-rename-buffer-and-file
 "C-x C-r" 'revert-buffer-quick
 "M-g m" 'consult-mark
 "C-x C-SPC" 'consult-global-mark
 "M-y" 'consult-yank-pop
 "C-o" 'crux-smart-open-line
 "C-k" 'crux-smart-kill-line
 "C-a" 'crux-move-beginning-of-line
 "M-l" nil
 "M-r" nil
 "M-R" 'query-replace
 "M-k" nil)

(general-unbind "C-x f")
(general-define-key
 :prefix "C-x f"
 "b" 'consult-bookmark
 "r" 'consult-recent-file
 "n" 'denote-open-or-create
 "o" 'my/find-file-org-directory
 "w" 'my/find-file-workspace
 "t" 'my/find-file-tramp
 "," 'my/find-file-emacs-configs)

(general-define-key
 :prefix "C-x p"
 "a" 'my/project-add
 "i" 'my/project-init)

;; Edit
;; ====
(general-define-key
 "M-z" 'zap-up-to-char
 "M-'" 'dabbrev-expand
 "C-M-'" 'dabbrev-completion)

;; Move & Window
;; =============
(general-define-key
 :keymaps 'override
 ;; Cursor Move
 "C-j" 'avy-goto-char
 "M-j" 'avy-goto-line
 "C-x =" 'er/expand-region
 "C-x -" 'er/contract-region
 "C-`" 'window-toggle-side-windows
 ;; Window Manipulation
 "M-1" 'my/smart-delete-other-windows
 "M-2" 'my/smart-split-window
 "M-`" 'other-window
 "M-O" 'window-swap-states
 )

;; Project
(general-unbind "C-q")
(general-define-key
 "C-x q" 'quoted-insert)

;; Shortcuts
;; =========
(general-define-key
 :keymaps 'override
 "C-x g" 'magit
 "C-x C-t" 'vterm
 "C-x m" 'compile
 "C-x C-m" 'recompile
 "C-x e" 'eshell
 "C-x x s" 'scratch-buffer
 "C-x x m" #'(lambda()(interactive)(switch-to-buffer "*Messages*"))
"s-<backspace>" 'backward-kill-word
 ;; Up/Down case
 "C-x C-u" 'crux-upcase-region
 "C-x C-l" 'crux-downcase-region
 "C-x M-c" 'crux-capitalize-region
 "s-d" 'my/osx-dict)

(general-define-key
 "C-; C-;" 'async-shell-command
 "C-; q r" 'query-replace
 "C-; c r" 'avy-copy-region
 "C-; r r" 'replace-regexp
 "C-; c q" 'chatgpt-query
 "C-; w c" 'whitespace-cleanup
 "C-; x g" 'gud-gdb
 "C-; s b" 'scratch-buffer)

(general-define-key
 "C-x l" 'eglot
 "C-c q" 'chatgpt-query)



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
 "r" 'rg
 "R" 'rg-menu
 "l" 'consult-line
 "M-l" 'consult-line-multi
 "M-g" 'consult-git-grep
 "M-o" 'loccur
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
 "C-." 'eglot-code-action-quickfix
 "C-c C-f" 'eglot-format-buffer
 "C-c C-r" 'eglot-rename
 "C-x l" 'eglot-reconnect)

;; Company
;; =======
(general-define-key
 :keymaps 'yas-minor-mode-map
 "C-c C-y" 'company-yasnippet)

(general-define-key
 :keymaps 'company-active-map
 "<return>" nil
 "<tab>" 'company-complete-common-or-cycle)

;; Dired
;; =====
(general-define-key
 :keymaps 'dired-mode-map
 "-" 'dired-up-directory
 "." 'my/cycle-dired-switches
 "C-c u" '0x0-dwim)

;; Project
;; =======
(general-define-key
 :keymaps 'project-prefix-map
 "p" 'my/project-switch
 "m" 'project-magit)

;; C Mode
;; ======
(general-define-key
 :keymaps 'c-mode-base-map
 "TAB" 'my/c-indent-complete)

;; HS minor mode
(general-define-key
 :keymaps 'hs-minor-mode-map
 "C-c h" 'hs-hide-all
 "C-c H" 'hs-show-all
 "<backtab>" 'hs-toggle-hiding)

;; Minibuffer
;; ==========
(general-define-key
 :keymaps 'minibuffer-mode-map
 "M-r" 'consult-history)

;; Org Capture & Roam
;; ==================
(general-define-key
 :keymaps 'org-mode-map
 "C-M-y" 'org-download-clipboard
 "M-n" 'org-next-visible-heading
 "M-p" 'org-previous-visible-heading)

;; Denote
(general-unbind "C-x n")
(general-define-key
 :prefix "C-x n"
 "f" 'denote-open-or-create
 "g" 'my/denote-grep
 "n" 'denote)

(general-define-key
 :keymaps 'flyspell-mode-map
 "C-." 'ispell-word)
