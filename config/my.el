(defun my/scroll-down (arg)
  "Move cursor down half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 3)))
    (next-line dist)))

(defun my/scroll-up (arg)
  "Move cursor up half a screen ARG times."
  (interactive "p")
  (let ((dist (/ (window-height) 3)))
    (previous-line dist)))

(defcustom my/dired-switches-list
  '("-lh" "-lah")
  "List of ls switches for dired to cycle among.")

(defun my/cycle-dired-switches ()
  "Cycle through the list `my/dired-switches-list' of switches for ls"
  (interactive)
  (setq my/dired-switches-list
        (append (cdr my/dired-switches-list)
                (list (car my/dired-switches-list))))
  (setq dired-listing-switches (car my/dired-switches-list))
  (dired-sort-other (car my/dired-switches-list)))

(defun my/find-file-org-directory ()
  (interactive)
  (let ((default-directory (file-name-concat org-directory "./")))
    (call-interactively 'find-file)))

(defun my/find-file-emacs-configs ()
  (interactive)
  (let ((default-directory (file-name-concat user-emacs-directory "config/")))
    (call-interactively 'find-file)))

(defun my/find-file-workspace ()
  (interactive)
  (let ((default-directory
         (if (file-remote-p default-directory)
             (tramp-make-tramp-file-name
              (tramp-dissect-file-name default-directory) "~/Workspace/")
           "~/Workspace/")))
    (call-interactively 'find-file)))

(defun my/find-file-tramp ()
  (interactive)
  (let ((default-directory "/scp:"))
  (call-interactively 'find-file)))

(defun my/toggle-window-size ()
  (interactive)
  (let ((wh (window-height))
        (half (/ (frame-height) 2))
        (quart (/ (frame-height) 4)))
    (if (>= half wh)
        (enlarge-window (- (* 3 quart) wh))
      (enlarge-window (- half wh)))))

(defun my/magit-pull-user-emacs-directory()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively 'magit-pull-from-pushremote)))

(defun my/is-tramp ()
  (file-remote-p default-directory))

(defun my/reload-user-init-file ()
  (interactive)
  (load user-init-file))

(defun my/vertico-toggle-height ()
  (interactive)
  (setq-local vertico-count (% (+ vertico-count 10) 20)))

(defun my/c-indent-complete()
  (interactive)
  (let (( p (point)))
    (c-indent-line-or-region)
    (when (= p (point))
      (call-interactively 'complete-symbol))))

(defun my/osx-dict ()
  "Look up a word in macOS Dictionary.app"
  (interactive)
  (browse-url (concat "dict:///" (url-hexify-string (current-word nil t)))))

(defun my/fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))

(defun my/denote-grep()
  "Use interactive grepping to search my notes"
  (interactive)
  (consult-ripgrep denote-directory))

(defun my/enable-word-wrap()
  (setq-local word-wrap t))

(defun my/smart-delete-other-windows ()
  "My smart delete other windows vertically"
  (interactive)
  (if (eq 2 (length (window-list)))
      (delete-other-windows)
    (delete-other-windows-vertically)))

(defun my/smart-split-window()
  "My smart split window"
  (interactive)
  (if (eq 1 (length (window-list)))
      (split-window-right)
    (split-window-below)))

(defun my/create-etags(dir-name)
  "Create etags"
  (interactive "DDirectory: ")
  (let ((default-directory dir-name))
    (shell-command
     (format "find . -type f \
-name \"*.[chCH]\" \
-o -iname \"*.cpp\" \
-o -iname \"*.h\" \
| etags -"))))


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
      ; (start-process-shell-command command (current-buffer) command)
      (switch-to-buffer-other-window (current-buffer)))
    ))

(defun my/project-recentf-find-file (&optional filter)
  "Find a recent file using `completing-read'.
When optional argument FILTER is a function, it is used to
transform recent files before completion."
  (interactive)
  (let* ((filter (if (functionp filter) filter #'abbreviate-file-name))
         (project-dir (project-root (project-current t)))
         (file (completing-read (format "Recentf %s: " )
                                (seq-filter #'(lambda (file-name)
                                                (file-in-directory-p file-name (project-root (project-current t))))
                                            (delete-dups (mapcar filter recentf-list)))
                                nil t)))
    (when file
      (find-file file))))

;; Examples
(defun my/hydra-print-project-list()
  (let* ((len (length project--list))
         (idx-list (number-sequence 1 len))
         (idx-prj-list (seq-mapn #'list idx-list (seq-map #'car project--list))))
    (seq-reduce (lambda(p c)(format "%s\n[%d] %s" p (car c) (cadr c)))
                idx-prj-list "")
  ))

(message (my/hydra-print-project-list))

(defhydra hydra-example
  (:color pink
          :pre (progn
                 (setq-local my/hydra-project-list-string (my/hydra-print-project-list))))
  "
%s(my/hydra-print-project-list)
"
  ("s" #'(lambda()(interactive)(setq-local my/name "hey")))
  ("f" find-file)
  ("b" forward-char)
  ("q" nil "cancel"))


(defhydra my/find-file-menu ()
  ;""
  ("r" #'crux-recentf-find-file))


(provide 'my)
