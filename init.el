(setq my/cloud-directory
	  (expand-file-name
	   (let ((path-1 "~/Library/Cloudstorage/Dropbox/")
			 (path-2 "~/Dropbox/"))
		 (if (file-exists-p path-1) path-1 path-2))))

(defvar my/config-files
  '("config/emacs.el"
	"config/packages.el"
	"config/my.el"
	"config/keybindings.el"
    "config/completion.el"
    "config/project.el"
    "config/editor.el"
    "config/text.el"
    "config/appearance.el"
	)
  "My configuration files")

(dolist (d '("./lisp/"))
  (let ((default-directory (concat user-emacs-directory d)))
	(add-to-list 'load-path default-directory)
	(normal-top-level-add-subdirs-to-load-path)))

;; Load
;; ====
(dolist (f my/config-files)
  (load (file-name-concat user-emacs-directory f)))
(put 'narrow-to-region 'disabled nil)
