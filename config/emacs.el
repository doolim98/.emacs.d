(require 'tramp)
(require 'eshell)
(require 'dired)
(require 'recentf)

(savehist-mode 1)
(show-paren-mode 1)
(desktop-save-mode 1)
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
 enable-recursive-minibuffers t
 shell-command-dont-erase-buffer 'end-last-out
 revert-without-query t
 auto-revert-interval 1)

(fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"

;; Garbage Collection
;; ==================
(setq garbage-collection-messages t)
(setq gc-cons-threshold (* 1024 1024 300))

;; TRAMP
;; =====
(setq enable-remote-dir-locals t)
(setq remote-file-name-inhibit-cache nil)
(setq remote-file-name-inhibit-locks t)
(setq tramp-default-method "scpx")
;; (setq tramp-ssh-controlmaster-options
;; 	  (concat
;; 	   "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
;; 	   "-o ControlMaster=auto -o ControlPersist=yes"))
(setq tramp-verbose 1)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; eshell
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

;; dired
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)
(setq dired-listing-switches "-lh") ;; Hide hidden files by default

;; recentf
(setq recentf-auto-cleanup 'never)
(setq-default recentf-max-menu-items 150
              recentf-max-saved-items 150)

(recentf-mode 1)
