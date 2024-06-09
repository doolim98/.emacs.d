;;; eplan.el --- Simple Planner

;;; Commentary:
;;

(require 'vtable)
(require 'tabulated-list)
(require 'cl-macs)
(require 'cl-lib)
(require 'org)
(require 'parse-time)

(define-derived-mode eplan-mode tabulated-list-mode "Eplan List"
  "Major mode for listing buffers and killing selected ones."
  ;; NOTE: Copied from Buffer-menu-mode, I can add some hook functions before revert the list
  (add-hook 'tabulated-list-revert-hook 'eplan-list--refresh nil t))

(setq eplan-mode-map
	  (define-keymap
		"k" 'my-kill-buffer
		"m" #'(lambda()(interactive)
				(message "Buffer: %s" (tabulated-list-get-id)))
		;; "a" 'eplan
		))

;; Date : default behavior
;; Description : mandatory
(cl-defstruct eplan-object
  ()
  date
  title
  description)

(defun eplan-list--refresh (&optional buffer-list old-buffer)
  (setq tabulated-list-use-header-line t)
  (setq tabulated-list-format
		(vector
		 ;; '("C" 1 t :pad-right 0)
		 ;; '("R" 1 t :pad-right 0)
		 ;; '("M" 1 t)
		 `("Buffer" 10 tabulated-list-entry-size-> :right-align t)
		 ;; `("Size" 10 tabulated-list-entry-size-> :right-align t)
		 `("Mode" 10 t)
		 ;; '("File" 1 t)
		 ))
  (setq tabulated-list-entries
		(cl-loop for buf in (buffer-list)
				 collect
				 (with-current-buffer buf
				   (list (current-buffer)
						 (vector (symbol-name major-mode)
								 (buffer-name buf)))))
		)
  (tabulated-list-init-header)
  )

(defun my-kill-buffer ()
  "Kill the selected buffer in buffer list." 
  (interactive)
  (let ((buf (tabulated-list-get-id)))
    (when (bufferp buf)
      (kill-buffer buf)
      (message "Buffer killed: %s" buf)
	  (tabulated-list-revert)
	  )))

(defun list-eplan()
  (interactive)
  (with-current-buffer (get-buffer-create "*eplan*")
	(eplan-mode)
	(tabulated-list-revert)
	(display-buffer (current-buffer))))

(provide 'eplan)

;;; eplan.el ends here
