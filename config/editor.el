(require 'prog-mode)
(require 'electric)
(require 'cc-styles)
(require 'eglot)
(require 'grammarly)
(require 'flymake-grammarly)

(setq eldoc-echo-area-use-multiline-p nil
	  flymake-no-changes-timeout 1)
(c-set-offset 'innamespace 0)
;; (setq eglot-ignored-server-capabilites '(:documentHighlightProvider))
;; (setq eglot-autoreconnect 1)

;; (add-hook 'c-mode-hook 'eglot-ensure)

;; Flymake
;; =======
(setq flymake-start-on-save-buffer t
	  flymake-no-changes-timeout nil)
(defun config-flymake-mode()
  (setq-local next-error-function 'flymake-goto-next-error))
(add-hook 'flymake-mode-hook 'config-flymake-mode)

;; Ispell & Dictionary
;; ===================
;; TOOD concern 'spell-fu' package
(setq ispell-program-name "aspell"
	  ispell-silently-savep t
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
      ispell-personal-dictionary (file-name-concat org-directory "./aspell.pws"))

;; I don't care :(
(setq grammarly-username "hojoon.lee@skku.edu")
(setq grammarly-password "Tltmxpa-l33t!")
(setenv "OPENAI_API_KEY" "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")
(electric-pair-mode 1)

(add-hook 'prog-mode-hook 'hs-minor-mode)
;; Auto Revert
;; ===========
(defun my/immediate-auto-revert-mode()
  (setq-local auto-revert-interval 1
			  auto-revert-remote-files t)
  (auto-revert-mode 1))
  
(add-hook 'image-mode-hook 'my/immediate-auto-revert-mode)

======= end
