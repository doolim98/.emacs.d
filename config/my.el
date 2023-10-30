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

(provide 'my)
