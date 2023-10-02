(require 'prog-mode)
(require 'electric)
(require 'cc-styles)
(require 'eglot)

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

;; Ispell & Dictionary
;; ===================
;; TOOD concern 'spell-fu' package
(setq ispell-program-name "aspell"
	  ispell-silently-savep t
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
      ispell-personal-dictionary (file-name-concat org-directory "./aspell.pws"))

(setq dictionary-server "localhost")

(electric-pair-mode 1)
