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


;; ChatGPT
;; =======



;; Dictionary
(defun my/read-word-at-point ()
  "READ CURRENT WORD."
  (interactive)
  (message "reading: %s" (thing-at-point 'word 'no-properties))
  (shell-command (format "espeak -v en-us %s" (thing-at-point 'word 'no-properties))))
