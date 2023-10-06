(if (eq system-type 'darwin)
	(setq-default org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org")
  (setq-default org-directory "~/org"))

(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil))
	  org-latex-image-default-height ""
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
	  org-download-image-attr-list
	  '(
		;; "#+attr_org: :width 300px"
		)
	  org-download-timestamp "%y%m%d-%H%M%S_"
	  org-download-screenshot-method "pngpaste %s")
;; (setq-default org-download-image-dir (concat org-directory "/img/"))

;; Org Capture
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Org ROAM
;; ========
(require 'org-roam-dailies)
(setq org-roam-directory (file-name-concat org-directory "./roam/"))
(setq org-roam-completion-everywhere nil)
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:*}" 'face 'org-tag)))
(setq org-roam-dailies-directory "journal/")
;; (setq org-roam-capture-templates
;; 	  '(("d" "default" entry "* ${title}
;; %?" :target
;; 		 (file+head "%<%Y%m%d%H%M%S>.org" "#+title: ${title}
;; ")
;; 		 :unnarrowed t)))

(add-hook 'org-mode-hook 'org-roam-db-autosync-enable)

(defun my/roam-grep()
  "Use interactive grepping to search my notes"
  (interactive)
  (consult-ripgrep org-roam-directory))
