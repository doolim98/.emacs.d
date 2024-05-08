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
(save-place-mode 0)
(repeat-mode 1)
(auto-save-visited-mode 0)

;; (setq package-install-upgrade-built-in t)



;; (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"

;; Use submodules as project
(setq project-vc-merge-submodules nil)

;; I only use git for version control
;; Used this configuration for faster tramp connection
(setq vc-handled-backends '(git))

;; TRAMP
;; =====


;; eshell
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
