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
  (let ((default-directory (file-name-concat user-emacs-directory "lisp/config-")))
	(call-interactively 'find-file)))

(provide 'my)
