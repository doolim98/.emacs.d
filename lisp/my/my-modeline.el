;;; my-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

(defun my-mode-line-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun prot-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))


(defmacro prot-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "prot-modeline-flymake-%s" type)) ()
     (when-let ((count (prot-modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type))))))


(prot-modeline-flymake-type error "X")
(prot-modeline-flymake-type warning "!")
(prot-modeline-flymake-type note "·" success)

(defvar-local my-mode-line-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         '(:eval "hello")
         ;;'(:eval (flymake--mode-line-counter 'error))
         '(:eval (flymake--mode-line-counter :error))
         '(:eval (flymake--mode-line-counter :warning))
         '(:eval (flymake--mode-line-counter :note))
         )
        )))

(defvar-local prot-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; See the calls to the macro `prot-modeline-flymake-type'
         '(:eval "hey")
         '(:eval (prot-modeline-flymake-error))
         '(:eval (prot-modeline-flymake-warning))
         '(:eval (prot-modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")


(dolist (construct '(;;prot-modeline-kbd-macro
                     ;;prot-modeline-narrow
                     ;;prot-modeline-input-method
                     ;;prot-modeline-buffer-status
                     ;;prot-modeline-window-dedicated-status
                     ;;prot-modeline-buffer-identification
                     ;;prot-modeline-major-mode
                     ;;prot-modeline-process
                     ;;prot-modeline-vc-branch
                     prot-modeline-flymake
                     ;;prot-modeline-eglot
                     ;; prot-modeline-align-right
                     ;;prot-modeline-notmuch-indicator
                     ;;prot-modeline-misc-info
                     ))
    (put construct 'risky-local-variable t))

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local prot-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

(provide 'my-modeline)
