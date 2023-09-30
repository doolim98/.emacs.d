(defvar my/config-files
  '("config/my.el"
    "config/emacs.el"
    "config/minimal-packages.el"
    "config/keybindings.el"
    "config/completion.el"
    "config/project.el"
    "config/prog-mode.el"
    "config/org.el"
    "config/appearance")
  "My configuration files")

(dolist (d '("./third-party/"))
  (let ((default-directory (concat user-emacs-directory d)))
	(add-to-list 'load-path default-directory)
	(normal-top-level-add-subdirs-to-load-path)))

;; Load
;; ====
(dolist (f my/config-files)
  (load (file-name-concat user-emacs-directory f)))
