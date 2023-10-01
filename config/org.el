(if (eq system-type 'darwin)
	(setq-default org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org")
  (setq-default org-directory "~/org"))

(setq org-pretty-entities nil
	  org-hide-emphasis-markers nil
	  org-image-max-width 500)

(add-hook 'dired-mode 'org-download-enable)
(setq qorg-download-method 'directory
	  org-download-image-dir "./img/"
	  org-download-heading-lvl nil
	  org-download-timestamp "%y%m%d-%H%M%S_"
	  org-download-screenshot-method "pngpaste %s")
;; (setq-default org-download-image-dir (concat org-directory "/img/"))
