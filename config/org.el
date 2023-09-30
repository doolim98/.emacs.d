(straight-use-package 'org)
(use-package org-download)
(use-package org-roam)
(require 'org)

(when (eq system-type 'darwin)
  (setq-default org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org"))

(setq org-pretty-entities nil
	  org-hide-emphasis-markers nil
	  org-image-max-width 500)

(require 'org-download)
(add-hook 'dired-mode 'org-download-enable)
(setq qorg-download-method 'directory
	  org-download-image-dir "images"
	  org-download-heading-lvl nil
	  org-download-timestamp "%Y%m%d-%H%M%S_"
	  org-download-screenshot-method "pngpaste %s")
(setq-default org-download-image-dir (concat org-directory "/img/"))
