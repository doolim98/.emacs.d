(use-package magit)

;; Project.el
;; ==========
(require 'project)
(require 'magit)
(setq project-switch-use-entire-map t)
(setq project-switch-commands
      '((project-dired "Dired" d)
		(project-magit "Magit" m)
        (project-eshell "Eshell" e)
        (project-shell "Shell" s)
        (project-compile "Compile" c)
        (project-async-shell-command "Async Shell Command" &)))

(defun my/project-switch(project)
  (interactive (list (project-prompt-project-dir)))
  (dired project))

(defun project-magit  ()
  (interactive)
  (let ((dir (project-root (project-current t))))
	(magit-status dir)))
(defun my/find-project (dir)
  (let ((override (locate-dominating-file dir ".dir-locals.el")))
    (if override
      (cons 'transient override)
      nil)))

(defun my/project-init()
  (interactive)
  (when (not (file-exists-p "./.dir-locals.el"))
	(make-empty-file ".dir-locals.el"))
  (my/project-add))

(defun my/project-add()
  (interactive)
  (when (not (project-current))
	(make-empty-file ".dir-locals.el"))
  (project-remember-project (project-current))
  (message "Added project %s" (project-root (project-current))))

(add-to-list 'project-find-functions 'my/find-project)


(defcustom my/project-commands
  '("./build.sh" "make")
  "List of project commands")

;; Compile
;; =======
(require 'compile)
(setq compilation-scroll-output t)
