(use-package magit)

;; Project.el
;; ==========
(require 'project)
(require 'magit)
(setq project-switch-use-entire-map t)
(setq project-switch-commands
      '((project-find-file "Find file" f)
        (project-dired "Dired" d)
        (project-vc-dir "VC-Dir" v)
		(project-magit "Magit" m)
        (project-eshell "Eshell" e)
        (project-shell "Shell" s)))

(defun project-magit  ()
  (interactive)
  (let ((dir (project-root (project-current t))))
	(magit-status dir)))
(defun my/find-project (dir)
  (let ((override (locate-dominating-file dir ".dir-locals.el")))
    (if override
      (cons 'transient override)
      nil)))

(defun my/project-add()
  (interactive)
  (when (not (project-current))
	(make-empty-file ".dir-locals.el"))
  (project-remember-project (project-current)))

(add-to-list 'project-find-functions 'my/find-project)

;; Compile
;; =======
(require 'compile)
(setq compilation-scroll-output t)
