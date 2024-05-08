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

(defun my/project-compile-command(command)
  "My project compile command"
  (interactive (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))))
  (let ((default-directory (project-root (project-current t)))
        (command-name command))
    (with-current-buffer (get-buffer-create (format "*%s$ %s*" (my/project-name) command-name))
      (compilation-mode 1)
      (compilation-start command)
      ;; (start-process-shell-command command (current-buffer) command)
      (switch-to-buffer-other-window (current-buffer)))))

(defun my/project-async-command()
  "My project async command"
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (command (read-shell-command "Project Command: "))
         (output-buffer (get-buffer-create (format "*Async <%s> %s" (my/project-name) command)))
         (proc (progn
                 (shell-command command output-buffer)
                 (get-buffer-process output-buffer))))
    (if (process-live-p proc)
        (set-process-sentinel proc #'(lambda ()(message "Good")))
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



(provide 'my-project)
