;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(use-package project
  :ensure nil
  :demand t
  :bind (("M-s M-s" . project-find-file)
         :map project-prefix-map
         ("m" . project-magit)
         ("d" . project-dired))
  :init
  (setq project-switch-commands
        '((project-find-file "Find file" f)
          (project-dired "Dired" d)
          (project-vc-dir "VC-Dir" v)
          (project-eshell "Eshell" e)
          (project-shell "Shell" s)))
  :config
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (project-roots)
  (setq consult-project-function
        (lambda (may-prompt)
          (when-let* ((project (project-current))
                      (project-root (car (project-roots (project-current))))
                      (is-not-home
                       (not (string= "/home/hjoll6/" (car (project-roots
                                                            (project-current)))))))
            project-root)))

  (defvar project-root-markers
    '(".git" "spago.dhall" "CMakeList.txt" "package.clj"
      "package.json" "mix.exs" "Project.toml" ".project" "Cargo.toml"
      "qlfile"))

  (defun my/project-find-root (path)
    (let* ((this-dir (file-name-as-directory (file-truename path)))
           (parent-dir (expand-file-name (concat this-dir "../")))
           (system-root-dir (expand-file-name "/")))
      (cond
       ((my/project-root-p this-dir) (cons 'transient this-dir))
       ((equal system-root-dir this-dir) nil)
       (t (my/project-find-root parent-dir)))))

  (defun my/project-root-p (path)
    (let ((results (mapcar (lambda (marker)
                             (file-exists-p (concat path marker)))
                           project-root-markers)))
      (eval `(or ,@ results))))

  (add-to-list 'project-find-functions #'my/project-find-root))

;;; COMPILATION
(use-package compile
  :defer t
  :hook (((c++-mode c-mode java-mode javascript-mode go-mode nroff-mode) . generic-compiler)
         (purescript-mode . spago-compiler))
  :bind (("C-x M-m" . compile)
         ("C-x C-m" . recompile))
  :init
  (defun has-makefile-p ()
    (or (file-exists-p "makefile")
	(file-exists-p "Makefile")))
  (defun spago-compiler ()
    (unless (has-makefile-p)
      (setq-local compile-command "spago run")))
  (defun generic-compiler ()
    (unless (has-makefile-p)
      (setq-local compile-command
		  (concat "compiler "
			  (when buffer-file-name
			    (shell-quote-argument buffer-file-name))))))
  :config
  (setq compilation-scroll-output t)
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (defun generic-compiler ()
    (unless (or (file-exists-p "makefile")
		(file-exists-p "Makefile"))
      (setq-local compile-command
		  (concat "compiler "
			  (if buffer-file-name
			      (shell-quote-argument
			       (file-name-sans-extension buffer-file-name)))))))
  (add-hook 'c++-mode-hook #'generic-compiler)
  )
