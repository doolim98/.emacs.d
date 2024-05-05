(require 'tramp)
(require 'eshell)
(require 'dired)
(require 'recentf)

(global-hl-line-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'mixed)

(desktop-save-mode 0)
(winner-mode 1)
(save-place-mode 1)
(repeat-mode 1)
(auto-save-visited-mode 1)

(setq package-install-upgrade-built-in t)

(setq-default
 tab-width 4
 history-length 1000
 create-lockfiles nil
 auto-save-default nil
 auto-save-timeout 5
 make-backup-files nil
 scroll-step 1 ;; not sure :(
 scroll-conservatively 1 ;; not sure too :(
 auto-window-vscroll t
 recenter-redisplay nil
 help-window-select t
 split-window-keep-point nil
 enable-recursive-minibuffers nil
 shell-command-dont-erase-buffer nil ; 'end-last-out
 revert-without-query t
 auto-revert-interval 1)

;; (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"

;; Use submodules as project
(setq project-vc-merge-submodules nil)

;; Garbage Collection
;; ==================
(setq garbage-collection-messages nil)
(setq gc-cons-threshold (* 10 1024 1024))

;; I only use git for version control
;; Used this configuration for faster tramp connection
(setq vc-handled-backends '(git))

;; TRAMP
;; =====
(setq enable-remote-dir-locals nil)
(setq remote-file-name-inhibit-cache nil)
(setq remote-file-name-inhibit-locks t)
(setq tramp-default-method "scp")
(setq tramp-use-scp-direct-remote-copying t)
(setq tramp-use-ssh-controlmaster-options t)
(setq tramp-ssh-controlmaster-options
	  (concat
	   "-o ControlPath=tramp.%%C "
	   "-o ControlMaster=auto -o ControlPersist=yes"))

(setq tramp-verbose 3)
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; eshell
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

;; dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lh") ;; Hide hidden files by default

;; recentf
