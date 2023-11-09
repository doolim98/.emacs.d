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
	  '(("improve 1st univ" . "Improve the clarity and coherence of my writing like a fist year american university student")
		("grammar" . "Could you check the grammar in this paragraph and suggest any corrections?")
		("improve" . "Improve the clarity and coherence of my writing.")
		("improve 3" . "Improve the clarity and coherence of my writing and suggest 3 writings")
		("rewrite 3" . "Rewrite my writing and suggest 3 writings")
		("cohesive" . "Can you improve this paragraph to make it more cohesive.")
		("bug" . "There is a bug in the following, please help me fix it.")
		("doc" . "Please write the documentation for the following.")
		("refactor" . "Please refactor the following.")
		("suggest" . "Please make suggestions for the following.")))

;; Latex
(defun my/find-TeX-master()
  (let* ((master-name "main.tex")
         (master-dir (locate-dominating-file "./" master-name)))
    (if master-dir
        (file-name-concat master-dir master-name)
      nil)))
(setq bibtex-files '("refer.bib"
                     "refers.bib"
                     "ref.bib"
                     "refs.bib"
                     "reference.bib"))

(defun my/LaTeX-mode-hook()
  (add-to-list 'TeX-command-list
               '("LaTeXmk" "latexmk -c -pdf" TeX-run-command t t :help "Run LaTeXmk")
               t)
  ;; (setq TeX-parse-self t)

  (setq reftex-default-bibliography
        `("refer.bib" "../refer.bib" "ref.bib" "../ref.bib" "refs.bib" "../refs.bib"))
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-mode t)

  (setq TeX-command-default "LaTeXmk")
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (setq TeX-master (my/find-TeX-master)))
(add-hook 'LaTeX-mode-hook #'my/LaTeX-mode-hook)
;; (setq-default TeX-master "master") ; All master files called "master".
