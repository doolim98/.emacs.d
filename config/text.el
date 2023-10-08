(require 'ox-md)
(if (eq system-type 'darwin)
	(setq-default org-directory (file-name-concat my/cloud-directory "org/"))
  (setq-default org-directory "~/org"))

(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil))
	  org-latex-image-default-width "0.6\\textwidth"
	  org-latex-image-default-scale "")

(setq org-pretty-entities nil
	  org-hide-emphasis-markers nil
	  org-image-actual-width '(200)
	  org-return-follows-link t
	  org-startup-truncated nil
	  org-export-allow-bind-keywords t)

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



;; denote
;; ======
(require 'denote)


;; Remember to check the doc strings of those variables.
(setq denote-directory (file-name-concat my/cloud-directory "notes/"))
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


(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
;; (add-hook 'org-mode-hook 'my/enable-word-wrap)

(defun my/org-mode-hook()
  (setq-local visual-fill-column-center-text t
			  fill-column 80)
  (visual-line-mode 1))
(add-hook 'org-mode-hook 'my/org-mode-hook)