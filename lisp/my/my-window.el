;;; my-window.el --- My Window Library
(defun my/select-window-by-number (n)
  "Select the Nth column window ordered from left to right."
  (let* ((cur-lc (window-left-column (selected-window)))
         (windows
         (sort (window-list)
               (lambda (w1 w2)
                 (let ((lc1 (window-left-column w1))
                       (lc2 (window-left-column w2))
                       (tl1 (window-top-line w1))
                       (tl2 (window-top-line w2)))
                   (< (+ (* tl1 12345) lc1) (+ (* tl2 12345) lc2)))
                 )))
         (nth-col-top-window (nth (1- n) windows)))
    (if (not (eq cur-lc (window-left-column nth-col-top-window)))
        (select-window nth-col-top-window)
      (my/cycle-windows-vertical-in-column))))

(defun my/cycle-windows-vertical-in-column ()
  "Cycle through windows that are vertically aligned with the current window."
  (interactive)
  (let* ((current-window (selected-window))
         (current-left (window-left-column current-window))
         (current-right (+ current-left (window-width current-window)))
         (windows-in-column (seq-filter (lambda (w)
                                          (and (<= current-left (+ (window-left-column w) (window-width w)))
                                               (>= current-right (window-left-column w))))
                                        (window-list nil 'no-minibuf))))
    ;; Sort the windows by their top position
    (setq windows-in-column (sort windows-in-column (lambda (w1 w2)
                                                      (< (window-top-line w1) (window-top-line w2)))))
    ;; Find the next window to select
    (select-window (or (cadr (memq current-window windows-in-column))  ; Next window in list
                       (car windows-in-column)))))  ; Or first if at end

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


(provide 'my-window)
