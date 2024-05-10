(defun my/is-window-system()
  (memq window-system '(mac ns x)))

(defun my-enable-savehist-mode()
  (savehist-mode 1)
  (setq-default history-length 1000))

(provide 'my-common)
