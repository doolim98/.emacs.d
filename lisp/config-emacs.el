(savehist-mode 1)
(show-paren-mode 1)
(desktop-save-mode 1)

(setq-default
 delete-pair-blink-delay 0
 tab-width 4
 blink-cursor-delay 0.0
 blink-cursor-interval 0.2
 blink-cursor-blinks 9999
 cursor-type 'box
 history-length 1000
 use-dialog-box nil
 delete-by-moving-to-trash t
 create-lockfiles nil
 auto-save-default nil
 inhibit-startup-screen t
 ring-bell-function 'ignore
 split-width-threshold 160
 split-height-threshold 120)

(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh"
	  shell-file-name "bash")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; ESHELL
(require 'eshell)
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

;; DIRED
(require 'dired)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-lh") ;; Hide hidden files by default

;; recentf
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(customize-set-value 'recentf-make-menu-items 150)
(customize-set-value 'recentf-make-saved-items 150)
(recentf-mode 1)

;; DABBREV
(require 'dabbrev)
(setq-default dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))


(provide 'config-emacs)
