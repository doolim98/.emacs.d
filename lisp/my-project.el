;;; my-project.el --- My project
;;; Requirements
(require 'project)
(require 'cl-macs)

;;; Commentary:
;; 


;;; Code:
;;; Functions
(cl-defun my-add-project-local-variable()
  (interactive)
  (let ((default-directory (project-root (project-current))))
	(call-interactively 'add-dir-local-variable)))

(defun my/project-name (&optional project)
  "Return the name for PROJECT.
If PROJECT is not specified, assume current project root."
  (when-let (root (or project (my/project-root)))
    (file-name-nondirectory
     (directory-file-name
      (file-name-directory root)))))

(defun my/project-root ()
  "Return the current project root."
  (when-let (project (project-current))
    (project-root project)))

(cl-defun my-compile (&optional (silent nil))
  (interactive)
  (let ((command (compilation-read-command (eval compile-command)))
		(buffer (get-buffer-create (format "*compilation<%s>*" (my/project-name)))))
	(with-current-buffer buffer
	  (compilation-mode 1)
	  (compilation-start command)
	  (setq-local compilation-finish-functions '((lambda(buffer string)(print "hello"))))
	  )))

(defun my/project-async-command()
  "My project async command"
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (command (read-shell-command "Project Command: "))
         (output-buffer (get-buffer-create (format "*%s <%s>*" command (my/project-name))))
         (proc (progn
                 (shell-command command output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc #'(lambda(buffer msg)(message "Good")))
      (message "No process running."))))

(defun my/project-recentf-find-file (&optional filter)
  "Find a recent file using `completing-read'.
When optional argument FILTER is a function, it is used to
transform recent files before completion."
  (interactive)
  (let* ((filter (if (functionp filter) filter #'abbreviate-file-name))
         (project-dir (project-root (project-current t)))
         (file
          (completing-read
           (format "Recentf %s: " )
           (seq-filter
            #'(lambda (file-name)
                (file-in-directory-p
                 file-name
                 (project-root (project-current t))))
            (delete-dups (mapcar filter recentf-list)))
           nil t)))
    (when file
      (find-file file))))

(defun my/project-compilation-buffer-name (name-of-mode)
  (cond ((or (eq major-mode (intern-soft name-of-mode))
             (eq major-mode (intern-soft (concat name-of-mode "-mode"))))
	 (buffer-name))
	(t
	 (concat "*" (downcase name-of-mode)
			 "<" (my/project-name) ">"
			 "*"))))

(defun my/project-compile ()
  "Run `recompile' in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively #'recompile)))

(defun my/project-compilation-buffer()
  "Show project's compilation buffer"
  (interactive)
  (let ((buf (get-buffer ()))))
  (display-buffer ())
  (call-interactively ))

;;; my-project.el ends here
(provide 'my-project)

