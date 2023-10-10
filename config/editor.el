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

;; ChatGPT
;; =======
(exec-path-from-shell-copy-env "OPENAI_API_KEY")
;; See https://github.com/ahmetbersoz/chatgpt-prompts-for-academic-writing
(setq chatgpt-code-query-map
	  '(("improve 1st univ" . "Improve the clarity and coherence of my writing, to a fist year american university level.")
		("grammar" . "Could you check the grammar in this paragraph and suggest any corrections?")
		("improve" . "Improve the clarity and coherence of my writing.")
		("improve 3" . "Improve the clarity and coherence of my writing and suggest 3 writings")
		("rewrite 3" . "Rewrite my writing and suggest 3 writings")
		("cohesive" . "Can you improve this paragraph to make it more cohesive.")
		("bug" . "There is a bug in the following, please help me fix it.")
		("doc" . "Please write the documentation for the following.")
		("refactor" . "Please refactor the following.")
		("suggest" . "Please make suggestions for the following.")))

(electric-pair-mode 1)

(defun my/prog-mode-hook ()
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; Auto Revert
;; ===========
(defun my/immediate-auto-revert-mode()
  (setq-local auto-revert-interval 1
			  auto-revert-remote-files t)
  (auto-revert-mode 1))

(add-hook 'image-mode-hook 'my/immediate-auto-revert-mode)

