;;; my.el --- My emacs scripts -*- lexical-binding: t -*-
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
      ;; (start-process-shell-command command (current-buffer) command)
      (switch-to-buffer-other-window (current-buffer)))))

(defun my/project-async-command()
  "My project async command"
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (command (read-shell-command "Project Command: "))
         (output-buffer (get-buffer-create (format "<%s> %s" (my/project-name) command)))
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


(defun my/reset-custom-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(require 'consult)
;; (defcustom consult-global-args
;;   "global --no-pager --color=never --ignore-case\
;;    --extended-regexp --line-number -I --grep"
(defcustom my/consult-global-args
  "global --color=never --ignore-case \
   --extended-regexp"
  "Command line arguments for global, see `consult-git-grep'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(my/reset-custom-var 'my/consult-global-args)

(defun my/consult--global-make-builder (paths)
  "Create grep command line builder given PATHS."
  (let ((cmd (consult--build-args my/consult-global-args)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input)))
        (append cmd paths)))
    ))
    ;; (lambda (input)
    ;;   ;; (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
    ;;   ;;              (flags (append cmd opts))
    ;;   ;;              (ignore-case (or (member "-i" flags) (member "--ignore-case" flags))))
    ;;     ;; (if (or (member "-F" flags) (member "--fixed-strings" flags))
    ;;     ;;     (cons (append cmd (list "-e" arg) opts paths)
    ;;     ;;           (apply-partially #'consult--highlight-regexps
    ;;     ;;                            (list (regexp-quote arg)) ignore-case))
    ;;       (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended ignore-case)))
    ;;         (when re
    ;;           (cons cmd ;; (append cmd
    ;;                 ;;         (cdr (mapcan (lambda (x) (list "-E" x)) re))
    ;;                 ;;         opts paths)
    ;;                 hl))))))
;; )
;;))


;;;###autoload
(defun my/consult-global (&optional dir initial)
  "Search with `git grep' for files in DIR with INITIAL input.
See `consult-grep' for details."
  (interactive "P")
  (consult--grep "Global" #'my/consult--global-make-builder dir initial))


;; Examples
(defun my/hydra-print-project-list()
  (let* ((len (length project--list))
         (idx-list (number-sequence 1 len))
         (idx-prj-list (seq-mapn #'list idx-list (seq-map #'car project--list))))
    (seq-reduce (lambda(p c)(format "%s\n[%d] %s" p (car c) (cadr c)))
                idx-prj-list "")
  ))


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

(defun my/select-window-by-number (n)
  "Select the Nth window ordered from left to right."
  (let ((windows
         (sort (window-list)
               (lambda (w1 w2)
                 (let ((lc1 (window-left-column w1))
                       (lc2 (window-left-column w2))
                       (tl1 (window-top-line w1))
                       (tl2 (window-top-line w2)))
                   (< (+ (* lc1 12345) tl1) (+ (* lc2 12345) tl2)))
                 ))))
    (if (<= n (length windows))
        (select-window (nth (1- n) windows))
      (if (split-window-right)
          (my/select-window-by-number n)))))

(defun my/select-window-0 ()
  (interactive)
  (my/select-window-by-number 0))

(defun my/select-window-1 ()
  (interactive)
  (my/select-window-by-number 1))

(defun my/select-window-2 ()
  (interactive)
  (my/select-window-by-number 2))

(defun my/select-window-3 ()
  (interactive)
  (my/select-window-by-number 3))


(defun my/select-window-4 ()
  (interactive)
  (my/select-window-by-number 4))

(defun my/select-tab-1 ()
  (interactive)
  (tab-select 1))
(defun my/select-tab-2 ()
  (interactive)
  (tab-select 2))
(defun my/select-tab-3 ()
  (interactive)
  (tab-select 3))
(defun my/select-tab-4 ()
  (interactive)
  (tab-select 4))


(defun my/open-preferred-shell ()
  "Open a shell in the current file's directory or the project root. Prefers zsh, then bash, then sh."
  (interactive)
  (let* ((default-directory
           (if (buffer-file-name)
               (file-name-directory (buffer-file-name))
             (or (projectile-project-root) default-directory))) ;; Use projectile or similar for project root
        (shell-file-name
         (or (executable-find "zsh")
             (executable-find "bash")
             (executable-find "sh"))))
    (shell (concat "shell-" (file-name-nondirectory shell-file-name)))))

(defun my/balance-windows ()
  "Balance windows automatically."
  (unless (or (one-window-p) (active-minibuffer-window))
    (balance-windows)))






;; (defun my/show-flymake-diagnostics-at-point ()
;;   "Show Flymake diagnostics for the current line in a tooltip."
;;   (interactive)
;;   (if (flymake-mode) ; Check if Flymake is enabled in the current buffer
;;       (let ((diagnostics (flymake-diagnostics (line-beginning-position) (line-end-position))))
;;         (message diagnostics)
;;         (if diagnostics
;;             (tooltip-show (mapconcat #'flymake-diagnostic-text diagnostics "\n"))
;;           (message "No Flymake diagnostics for the current line")))
;;     (message "Flymake is not enabled in this buffer")))

;; (define-key flymake-mode-map (kbd "C-c ! m") 'my/show-flymake-diagnostics-at-point)


(provide 'my)
