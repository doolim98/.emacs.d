;;; my-latex.el --- My Latex config -*- lexical-binding: t -*-

(defun my-latex/find-TeX-master()
  (let* ((master-name "main.tex")
         (master-dir (locate-dominating-file "./" master-name)))
    (if master-dir
        (file-name-concat master-dir master-name)
      nil)))

(defun my-latex/revert-pdf-buffers ()
  (progn
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (string-match "\\.pdf\\'" (buffer-name))
                   (file-exists-p (buffer-file-name)))
          (revert-buffer t t t))))))

(defun my/compilation-finish-function (buffer msg)
  (progn
    (my/revert-pdf-buffers)))

(add-hook 'compilation-finish-functions 'my/compilation-finish-function)

(defun my/LaTeX-mode-hook()
  (setq TeX-output-dir "./build")
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server nil)
  (setq TeX-master (my/find-TeX-master))

  (setq eglot-workspace-configuration '((digestif . (:config (:data_dirs ("./" "./build"))))))
  (eglot-ensure)

  (auto-save-mode 0)
  (auto-revert-mode 0)
  (auto-save-visited-mode 0)
  (electric-indent-local-mode -1)

  (setq-local compile-command "latexmk;echo done!")
  )


(defun my/insert-string-at-buffer-top (string)
  "Insert a specific STRING at the top of the current buffer."
  (save-excursion  ; Preserve the point's original position
    (goto-char (point-min))  ; Move to the beginning of the buffer
    (insert string)  ; Insert the specified string
    (insert "\n")))  ; Optionally, insert a newline after the string

(defun my/latex-save-and-compile()
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (save-some-buffers t)
    (compile "latexmk"))
  )


(defun my/project-latex-compile-command()
  "My async latexmk "
  (interactive)
  (my/project-compile-command "latexmk -C;latexmk;latexmk -pvc -view=none")
  )

(add-hook 'LaTeX-mode-hook #'my/LaTeX-mode-hook)


(provide 'my-latex)
