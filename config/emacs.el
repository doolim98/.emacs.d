(require 'tramp)
(require 'eshell)
(require 'dired)
(require 'recentf)

(savehist-mode 1)
(show-paren-mode 1)
(desktop-save-mode 1)
(winner-mode 1)
(save-place-mode 1)

(setq-default
 tab-width 4
 history-length 1000
 delete-by-moving-to-trash t
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 scroll-step 1
 scroll-conservatively 10000
 auto-window-vscroll nil
 recenter-redisplay nil)

(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"

;; TRAMP
(setq tramp-default-method "ssh"
	  shell-file-name "bash")
(setq tramp-ssh-controlmaster-options
	  (concat
	   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
	   "-o ControlMaster=auto -o ControlPersist=yes"))
(setq tramp-verbose 2)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; ESHELL
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

;; DIRED
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lh") ;; Hide hidden files by default

;; recentf
(setq recentf-auto-cleanup 'never)
(customize-set-value 'recentf-make-menu-items 150)
(customize-set-value 'recentf-make-saved-items 150)
(recentf-mode 1)
