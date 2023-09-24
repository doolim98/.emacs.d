
(define-derived-mode
    my-list-mode tabulated-list-mode "My List Mode"
    "My List Mode"
    (setq tabulated-list-entries 'my-list-entries)
    (setq tabulated-list-format)
    (tabulated-list-init-header))

(provide my-list-mode)
