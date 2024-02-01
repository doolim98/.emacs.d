(require 'ox-md)
(if (eq system-type 'darwin)
	(setq-default org-directory (file-name-concat my/cloud-directory "org/"))
  (setq-default org-directory "~/org"))

(setq org-cite-global-bibliography `(,(file-name-concat org-directory "/refer.bib")))

(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil))
	  org-latex-image-default-width "0.6\\textwidth"
	  org-latex-image-default-scale "")

(setq org-pretty-entities nil
	  org-hide-emphasis-markers nil
	  org-image-actual-width '(200)
	  org-return-follows-link t
	  org-startup-truncated nil
	  org-export-allow-bind-keywords t)

(setq org-file-apps
      '((auto-mode . emacs)
        (directory . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . emacs)))

(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(add-hook 'dired-mode 'org-download-enable)
(setq-default org-download-method 'directory
	  org-download-image-dir "./img/"
	  org-download-heading-lvl nil
	  org-download-image-attr-list '()
	  org-download-screenshot-method "pngpaste %s")

;; Export
(setq org-latex-pdf-process
	  '(
		"mkdir -p .tmp/; latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=.tmp/ %f; mv %f .tmp/; mv .tmp/%b.pdf %o"
		))
(setq org-export-with-toc nil)
(setq org-md-headline-style 'atx)

;; Org Capture
(setq org-default-notes-file (concat org-directory "/notes.org"))


;;;; Ispell & Dictionary
(setq ispell-program-name "aspell"
	  ispell-silently-savep t
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
      ispell-personal-dictionary (file-name-concat org-directory "./aspell.pws"))



;; denote
;; ======
(require 'denote)

;; Remember to check the doc strings of those variables.
(setq denote-directory (file-name-concat my/cloud-directory "notes/"))
(setq denote-dired-directories `(,denote-directory))

(setq denote-known-keywords '("emacs" "reference"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil)
(setq denote-prompts '(title keywords))
(setq denote-org-front-matter
	  "#+title: %1$s\n"	  )

;; Pick dates, where relevant, with Org's advanced interface:
(setq denote-date-prompt-use-org-read-date t)

(setq denote-allow-multi-word-keywords t)

(defun my/org-mode-hook()
  (setq-local visual-fill-column-center-text t
			  fill-column 80)
  (visual-line-mode 1))
(add-hook 'org-mode-hook 'my/org-mode-hook)

;; ChatGPT
;; =======
(exec-path-from-shell-copy-env "OPENAI_API_KEY")
;; See https://github.com/ahmetbersoz/chatgpt-prompts-for-academic-writing
(setq chatgpt-code-query-map
	  '(
		("improve" . "You are my english writing assistance.
Improve the coherence and cohesivity of my writing.
Also, do not change the latex syntax.")
        ("academic" . "You are my computer-engineering paper writer.
Improve the coherence and cohesivity of my writing in academic style.
Also, do not change the latex syntax.")
        ("short" . "You are my english writing assistance.
Make my sentences shorter and more concise by using appropriate adjectives and nouns.")
        ("naming" . "You are my english writing assistance.
I want to find good name of the following description.
Suggest me 10 names")
		("bug" . "There is a bug in the following, please help me fix it.")
		("doc" . "Please write the documentation for the following.")
		("refactor" . "Please refactor the following.")
		("suggest" . "Please make suggestions for the following.")))



;; Dictionary
(defun my/read-word-at-point ()
  "READ CURRENT WORD."
  (interactive)
  (message "reading: %s" (thing-at-point 'word 'no-properties))
  (shell-command (format "espeak -v en-us %s" (thing-at-point 'word 'no-properties))))
