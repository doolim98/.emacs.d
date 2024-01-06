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

(defun my/LaTeX-mode-hook()
  (add-to-list 'TeX-command-list
               '("LaTeXmk" "latexmk -c -pdf" TeX-run-command t t :help "Run LaTeXmk")
               t)
  ;; (setq TeX-parse-self t)

  ;; (setq reftex-default-bibliography
  ;;       `("refer.bib" "../refer.bib" "ref.bib" "../ref.bib" "refs.bib" "../refs.bib"))
  ;; (setq reftex-plug-into-AUCTeX t)
  ;; (setq reftex-mode t)

  ;; (setq TeX-command-default "LaTeXmk")
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  (setq TeX-master (my/find-TeX-master))

  (setq eglot-workspace-configuration '((texlab . (:rootDirectory "./"))))
  (eglot-ensure)
  )

(defun my/project-latex-compile-command()
  "My async latexmk "
  (interactive)
  (my/project-compile-command "latexmk -C;latexmk -pdf;latexmk -pvc -view=none")
  )

(add-hook 'LaTeX-mode-hook #'my/LaTeX-mode-hook)
