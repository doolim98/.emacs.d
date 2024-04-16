;;; latex.el --- My Latex config -*- lexical-binding: t -*-

;; (defun biblio-dblp--forward-bibtex (metadata forward-to)
;;   "Forward BibTeX for DBLP entry METADATA to FORWARD-TO."
;;   (let* ((source-url (biblio-alist-get 'url metadata))
;;          (url (replace-regexp-in-string "/rec/" "/rec/bib1/" source-url t t)))
;;     (biblio-url-retrieve url (biblio-generic-url-callback
;;                               (lambda () ;; No allowed errors, so no arguments
;;                                 "Parse DBLP BibTeX results."
;;                                 (funcall forward-to
;;                                          (biblio-response-as-utf-8)))))))

(defun my/find-TeX-master()
  (let* ((master-name "main.tex")
         (master-dir (locate-dominating-file "./" master-name)))
    (if master-dir
        (file-name-concat master-dir master-name)
      nil)))

(setq bibtex-files '("refer.bib"
                     "refers.bib"
                     "ref.bib"
                     "refs.bib"
                     "reference.bib"))

(defun my/revert-pdf-buffers ()
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

;; (defun my/toggle-window-dedicated ()
;;   "Toggle whether the current window is dedicated to its current buffer and highlight modeline."
;;   (interactive)
;;   (let ((window (selected-window))
;;         (frame (selected-frame)))
;;     (set-window-dedicated-p window (not (window-dedicated-p window)))
;;     (if (window-dedicated-p window)
;;         (progn
;;           ;; Change modeline background for dedicated window
;;           (set-face-background 'mode-line "black" frame)
;;           (set-face-foreground 'mode-line "white" frame))
;;       ;; Revert modeline color for non-dedicated window
;;       (progn
;;         (set-face-background 'mode-line (face-background 'default) frame)
;;         (set-face-foreground 'mode-line (face-foreground 'default) frame)))
;;     (message "Window %sdedicated to %s"
;;              (if (window-dedicated-p window) "" "not ")
;;              (buffer-name))))


(add-hook 'LaTeX-mode-hook #'my/LaTeX-mode-hook)


;; (defun my/project-compile-latexmk-and-revert-pdf ()
;;   "Compile the current LaTeX file and revert any open PDF buffers."
;;   (interactive)
;;   ;; Check if the major mode is LaTeX to avoid running on other files
;;   (when (eq major-mode 'latex-mode)

;;     ;; Compile the LaTeX file. Change "pdflatex" to your preferred compiler command.
;;     (let ((default-directory (project-root (project-current t)))
;;           (compile-command "latexmk"))
;;       (compile compile-command))
;;     (message "Compilation started...")))
