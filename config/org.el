(if (eq system-type 'darwin)
	(setq-default org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org")
  (setq-default org-directory "~/org"))

(setq org-pretty-entities nil
	  org-hide-emphasis-markers nil
	  org-image-actual-width nil
	  org-return-follows-link t)
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(add-hook 'dired-mode 'org-download-enable)
(setq-default org-download-method 'directory
	  org-download-image-dir "./img/"
	  org-download-heading-lvl nil
	  org-download-image-attr-list
	  '("#+attr_html: :width 80% :align center"
		"#+attr_org: :width 300px")
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
(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
(setq org-roam-dailies-directory "journal/")
(org-roam-db-autosync-mode)
