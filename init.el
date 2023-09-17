;; Based on https://github.com/Gavinok/emacs.d/blob/main/init.el

;;; ASYNC
;; Emacs look SIGNIFICANTLY less often which is a good thing.
;; asynchronous bytecode compilation and various other actions makes
(use-package async
  :ensure t
  :defer t
  :init
  (dired-async-mode 1))

(use-package dwim-shell-command
  :ensure t :demand t
  :bind (([remap dired-do-shell-command] . dwim-shell-command))
  :config
  (require 'dwim-shell-commands))

(use-package savehist
  :defer 2
  :init (savehist-mode t))

(use-package repeat
  :defer 10
  :init
  (repeat-mode +1))

(use-package custom-variables
  :straight nil
  :ensure nil :no-require t   :demand t
  :init
  (cl-defmacro let-regex ((bindings (string regex)) &body body)
  "Macro for creating BINDINGS to captured blocks of REGEX found in a STRING.
BINDINGS: A list of different symbols to be bound to a captured section of the regex
STRING: The string the regex is searching through
REGEX: Regex used to match against the string

If no binding is captured section of regex is found for a BINDING an error is signaled
   ;; Example usage
   (let-regex ((h w) (\"hello world\" \"\\(hello\\) \\(world\\)\"))
                (message \"result was %s then %s\" h w))"
  (let ((holder (gensym)))
    `(let ((,holder (with-temp-buffer
                      (insert ,string)
                      (beginning-of-buffer)
                      (search-forward-regexp ,regex nil nil)
                      (let ((i 0))
                        (mapcar (lambda (_a)
                                  (setq i (+ i 1))
                                  (match-string i))
                                '( ,@bindings))))))
       (let ,(mapcar (lambda (binding)
                       `(,binding (or (pop ,holder)
				      (error "Failed to find binding for %s"
                                             ',binding))))
                     bindings)
         ,@body))))
  (defvar my/is-terminal
    (not window-system)
    "Truthy value indicating if Emacs is currently running in a terminal.")
  (defvar my/my-system
    (if (string-equal user-login-name "gavinok")
        t
      nil)
    "Non-nil value if this is my system."))

(use-package custom-functions
  :straight nil
  :ensure nil :no-require t
  :bind (([remap scroll-up-command] . my/scroll-down)
         ([remap scroll-down-command].  #'my/scroll-up)
         ("C-M-&"   . my/shell-command-on-file)
         ("C-x O"   . other-other-window)
         ("C-x n a" . my/increment-number-at-point)
         ("C-x n x" . my/decrement-number-at-point)
         ("C-c d"   . my/next-fn)
         :repeat-map my/next-fn-map
         ("d" . my/next-fn)
         :map image-mode-map
         ("&"       . my/shell-command-on-file)
         :repeat-map change-number-repeat-map
         ("a" . my/increment-number-at-point)
         ("x" . my/decrement-number-at-point))
  :init
  (defun my/next-fn (&optional arg)
    (interactive "P")
    (apply (if arg
               #'text-property-search-backward
             #'text-property-search-forward)
           'face
           (cond
            ((eql major-mode 'haskell-mode) 'haskell-definition-face)
            (T                              'font-lock-function-name-face))
           nil))

  (defun my/change-number-at-point (change increment)
    (search-forward-regexp (rx digit)) ; Go to the closest number
    (let ((number (number-at-point))
          (point (point)))
      (when number
        (progn
          (forward-word)
          (search-backward (number-to-string number))
          (replace-match (number-to-string (funcall change number increment)))
          (goto-char (- point 1))))))

  (defun my/increment-number-at-point (&optional increment)
    "Increment number at point like vim's C-a"
    (interactive "p")
    (my/change-number-at-point '+ (or increment 2)))

  (defun my/decrement-number-at-point (&optional increment)
    "Decrement number at point like vim's C-x"
    (interactive "p")
    (my/change-number-at-point '- (or increment 1)))

  (defun my/scroll-down (arg)
    "Move cursor down half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (next-line dist)))

  (defun my/scroll-up (arg)
    "Move cursor up half a screen ARG times."
    (interactive "p")
    (let ((dist (/ (window-height) 2)))
      (previous-line dist)))

  (defun my/shell-command-on-file (command)
    "Execute COMMAND asynchronously on the current file."
    (interactive (list (read-shell-command
                        (concat "Async shell command on " (buffer-name) ": "))))
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (async-shell-command (concat command " " filename))))

  (defun eqn-to-tex (eqn-expression)
    "Takes a eqn expression as a string string EQN-EXPRESSION and
returns the equivalent latex version."
    (calc-eval `(,eqn-expression
	         calc-simplify-mode none
	         calc-language eqn)
	       'push)
    (calc-eval '(1
	         calc-simplify-mode none
	         calc-language latex)
	       'top))

  (defun echo-eqn-to-tex (eqn-expr &optional arg)
    "Takes an eqn expression eqn-expr and prints a message with the
latex version of it."
    (interactive "sEnter eqn here: ")
    (message (eqn-to-tex eqn-expr)))

  (defun eqn-to-tex-region (start end)
    "Replaces the active region containing a eqn expression and
replaces it with the Latex equivalent."
    (interactive "r")
    (let ((converted-expr (eqn-to-tex (filter-buffer-substring start end))))
      (kill-region start end)
      (insert converted-expr)))

  (defun all-history ()
    "Command for getting command history from basically every source out
     there.

     No more \"Where did I call that again?\" going off in your head"
    (interactive)
    (flet ((file->lines (file-name)
                        (split-string
                         (with-temp-buffer
                           (insert-file-contents-literally file-name)
                           (buffer-substring-no-properties (point-min) (point-max))
                           "\n"))))
      (completing-read
       "All History: "
       (append shell-command-history
               compile-history
               (when (boundp 'eshell-history-file-name)
                 (file->lines eshell-history-file-name))
               (file->lines "~/.bash_history")))))

  (defun gist-from-region (BEG END fname desc &optional private)
    "Collect the current region creating a github gist with the
filename FNAME and description DESC.
If the optional argument PRIVATE is non-nil then the gist will be
made private. Otherwise the gist will be default to public.

Depends on the `gh' commandline tool"
    (interactive (list (mark) (point)
                       (read-string "File Name: ")
                       (read-string "Description: ")
                       current-prefix-arg))
    (let ((proc (make-process :name "Gist Creation"
                              :buffer "*Gist URL*"
                              :command (cl-remove :skip
                                                  (list "gh" "gist" "create"
                                                        "--filename" fname
                                                        "--desc" desc
                                                        (if private
                                                            :skip
                                                          "--public")
                                                        "-"))
                              :sentinel (lambda (process event)
                                          "Listens for process finish and prints the gist's URL"
                                          (when (string-match "finis.*" event)
                                            (message "Gist for file %s created at %s"
                                                     fname
                                                     (with-current-buffer (process-buffer process)
                                                       (goto-char (point-max))
                                                       (thing-at-point 'line))))))))

      (process-send-string proc (buffer-substring BEG END))
      (process-send-eof proc)
      (process-send-eof proc)))
  )


(use-package emacs
  :ensure nil
  :defer nil
  :bind (("C-c w"   . fixup-whitespace)
         ("M-z"     . zap-up-to-char)
         ("C-x S"   . shell)
         ("C-x M-t" . transpose-regions)
         ("M-1" . delete-other-windows)
         ("M-2" . split-window-below)
         ("M-3" . split-window-right)
	 )
  :config
  (setq-default delete-pair-blink-delay 0)

  (setq-default history-length 1000
                use-dialog-box nil
                delete-by-moving-to-trash t
                create-lockfiles nil
                auto-save-default nil
                inhibit-startup-screen t
                ring-bell-function 'ignore)

  ;;;; UTF-8
  (prefer-coding-system 'utf-8)

  ;;;; Remove Extra Ui
  (fset 'yes-or-no-p 'y-or-n-p)    ; don't ask to spell out "yes"
  (show-paren-mode 1)              ; Highlight parenthesis

  ;;TRAMP
  (setq tramp-default-method "ssh"
        shell-file-name "bash")

  ;; recentf
  (recentf-mode t)
  (customize-set-value 'recentf-make-menu-items 150)
  (customize-set-value 'recentf-make-saved-items 150)
  )

(use-package unified-marks
  :straight nil
  :ensure nil :no-require t
  :custom
  (global-mark-ring-max 256)
  (set-mark-command-repeat-pop 256)
  :init
  ;; Unify Marks
  (defun my/push-mark-global (&optional location nomsg activate)
    "Always push to the global mark when push-mark is called"
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history
       'global-mark-ring (copy-marker (mark-marker))
       global-mark-ring-max t)
      (when old
        (set-marker old nil))))
  (advice-add 'push-mark :after #'my/push-mark-global))

(use-package crux
  :ensure t
  :bind (("C-x w v" . crux-swap-windows)
         ("C-S-o"   . crux-smart-open-line-above)
         ("C-o"     . crux-smart-open-line)
         ("C-x B"   . my/org-scratch)
         :map dired-mode-map
         ("O" . crux-open-with))
  :config
  (defun my/org-scratch ()
    (interactive)
    (let ((initial-major-mode 'org-mode))
      (crux-create-scratch-buffer))))

;;; COMPLETION
(use-package vertico
  :init
  ;; Enable vertico using the vertico-flat-mode
  (require 'vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  (use-package orderless
    :commands (orderless)
    :custom (completion-styles '(orderless flex))
    ;; Allow tramp completion
    (completion-category-overrides '((file (styles basic partial-completion)))))

  (use-package marginalia
    :custom
    (marginalia-annotators
     '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))
  (vertico-mode t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;;;; Extra Completion Functions
(use-package consult
  :after vertico
  :bind (("C-x b"       . consult-buffer)
         ("C-x C-k C-k" . consult-kmacro)
         ("M-y"         . consult-yank-pop)
         ("M-g g"       . consult-goto-line)
         ("M-g M-g"     . consult-goto-line)
         ("M-g f"       . consult-flymake)
         ("M-g i"       . consult-imenu)
         ("M-s l"       . consult-line)
         ("M-s L"       . consult-line-multi)
         ("M-s u"       . consult-focus-lines)
         ("M-s g"       . consult-ripgrep)
         ("M-s M-g"     . consult-ripgrep)
         ("C-x C-SPC"   . consult-global-mark)
         ("C-x M-:"     . consult-complex-command)
         ("C-c n"       . consult-org-agenda)
         ("C-c m"       . my/notegrep)
         :map help-map
         ("a" . consult-apropos)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (defun my/notegrep ()
    "Use interactive grepping to search my notes"
    (interactive)
    (consult-ripgrep org-directory))
  (recentf-mode t))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-j" . consult-dir)
         ;; :map minibuffer-local-completion-map
         :map vertico-map
         ("C-x C-j" . consult-dir)))
(use-package consult-recoll
  :bind (("M-s r" . counsel-recoll)
         ("C-c I" . recoll-index))
  :init
  (setq consult-recoll-inline-snippets t)
  :config
  (defun recoll-index (&optional arg) (interactive)
    (start-process-shell-command "recollindex"
                                 "*recoll-index-process*"
                                 "recollindex")))

(use-package embark
  :ensure t
  :bind
  ;; pick some comfortable binding
  (("M-."                     . embark-act)
   ("C-<escape>"              . embark-act)
   ([remap describe-bindings] . embark-bindings)
   :map embark-file-map
   ("C-d"                     . dragon-drop)
   :map embark-defun-map
   ("M-t" . chatgpt-gen-tests-for-region)
   :map embark-general-map
   ("M-c" . chatgpt-prompt)
   :map embark-region-map
   ("?"   . chatgpt-explain-region)
   ("M-f" . chatgpt-fix-region)
   ("M-f" . chatgpt-fix-region))
  :custom
  (embark-indicators
   '(embark-highlight-indicator
     embark-isearch-highlight-indicator
     embark-minimal-indicator))
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq embark-prompter 'embark-completing-read-prompter)
  :config
  (defun search-in-source-graph (text))
  (defun dragon-drop (file)
    (start-process-shell-command "dragon-drop" nil
                                 (concat "dragon-drop " file))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (:all embark consult)
  :demand t
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



;;; Git
(use-package magit
  :bind (("C-x g" . magit-status)
         :map project-prefix-map
         ("m" . project-magit))
  :commands (magit project-magit)
  :config
  (add-to-list 'project-switch-commands
               '(project-magit "Magit" m))
  (defun project-magit  ()
    (interactive)
    (let ((dir (project-root (project-current t))))
      (magit-status dir))))
(use-package forge :ensure t :after magit)
(use-package ediff
  :after (magit vc)
  :commands (ediff)
  :init
  ;; multiframe just doesn't make sense to me
  (with-eval-after-load 'winner
    (add-hook 'ediff-quit-hook 'winner-undo))
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;; VTERM AND ESHELL
(use-package vterm
  :bind (("C-x t" . vterm)
         :map vterm-mode-map
         ("M-p" . vterm-send-up)
         ("M-n" . vterm-send-down))

  :commands vterm
  :custom (vterm-max-scrollback 10000)
  :init (when my/my-system
          (setq term-prompt-regexp ".*ᛋ")))

;; (use-package with-editor
;;   :hook ((shell-mode-hook eshell-mode-hook term-exec-hook vterm-exec-hook)
;;          . with-editor-export-editor)
;;   :bind (([remap async-shell-command] . with-editor-async-shell-command)
;;          ([remap shell-command] . with-editor-shell-command)))

(use-package eshell
  :bind ("C-x E" . eshell))

(use-package em-alias
  :straight nil
  :ensure nil
  :after eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "ee" "find-file-other-window $1")
              (eshell/alias "v" "view-file $1")
              (eshell/alias "o" "crux-open-with $1"))))

(use-package em-term
  :straight nil
  :ensure nil
  :after eshell
  :config
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-options '("ghcup" "tui"))
  (add-to-list 'eshell-visual-commands '("htop" "top" "git" "log" "diff"
                                         "show" "less" "nix")))

(use-package eshell
  :commands eshell
  :config
  (setq eshell-destroy-buffer-when-process-dies t))

(use-package fish-completion
  :demand t
  :config
  (global-fish-completion-mode))

;;;; Code Completion
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto nil)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

(use-package corfu-terminal
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

;; More accurate color representation than ansi-color.el
(use-package xterm-color
  :ensure t
  :after esh-mode
  :config
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color"))

;; Load project related configurations
(load (concat user-emacs-directory "lisp/config-project.el"))

;;; DIRED
(use-package dired
  :straight nil
  :ensure nil
  :commands (dired)
;;   :hook (
;;	 (dired-mode . hl-line-mode)
;        (dired-mode . dired-omit-mode)
;; 	 )
  :bind (:map dired-mode-map
              ("-" . dired-up-directory)
	      ("." . cycle-dired-switches))
  :init
  (setq dired-bind-jump nil)
  :config

  
  ;;;;; Hide . and .. in dired
  (setq dired-omit-files
        (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))
  ;; prevent opening extra dired buffers
  ;; emacs 28
  (setq dired-kill-when-opening-new-dired-buffer t)

  (setq dired-listing-switches "-alh")
  (defcustom list-of-dired-switches
  '("-lh" "-lah")
  "List of ls switches for dired to cycle among.")

  (defun cycle-dired-switches ()
    "Cycle through the list `list-of-dired-switches' of switches for ls"
    (interactive)
    (setq list-of-dired-switches
          (append (cdr list-of-dired-switches)
                  (list (car list-of-dired-switches))))
    (setq dired-listing-switches (car list-of-dired-switches))
    (dired-sort-other (car list-of-dired-switches)))
  )

(use-package sqlite3)

(use-package fontaine
  :unless my/is-terminal
  :config
  (setq fontaine-presets
        '((regular
           :default-height 140)
          (small
           :default-height 110)
          (large
           :default-weight semilight
           :default-height 180
           :bold-weight extrabold)
          (extra-large
           :default-weight semilight
           :default-height 210
           :line-spacing 5
           :bold-weight ultrabold)
          (t                        ; our shared fallback properties
           :default-family "Fira Code"
	   :bold-weight bold
           :italic-family nil
           :italic-slant italic
	   ;; :line-height 2.0
           ;; :line-spacing 0.0
	   )))
  (fontaine-set-preset 'regular)
  (defun set-bigger-spacing ()
    (setq-local default-text-properties '(line-spacing 0.2 line-height 1.2)))
  (add-hook 'text-mode-hook 'set-bigger-spacing)
  (add-hook 'prog-mode-hook 'set-bigger-spacing)
  (add-hook 'org-mode-hook 'set-bigger-spacing)
  (add-hook 'eshell-mode-hook 'set-bigger-spacing)
  )

;; Appearance
(use-package modus-themes
  :init
  :config
  (load-theme 'modus-operandi t)
  (setq modus-themes-bold-constructs t
	modus-themes-italic-constructs t)
  )

;; Org Mode
(use-package org-bullets
  :config
  (setq org-bullets-bullet-list
	  '(;;; Large
	    ;; "◉"
	    "●"
	    "○"
	    "◆"
	    "◇"
	    ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
	    ;; ► • ★ ▸
	    )
	  )
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
)


(use-package org
  :straight nil
  :config
  (message "hello orgmode")
  (setq org-pretty-entities t
	org-hide-emphasis-markers t
	)
  (custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-1 :height 1.2))))
  '(org-level-3 ((t (:inherit outline-1 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-1 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-1 :height 1.0))))
  (set-face-attribute 'org-document-title nil :height 2.0))
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Helvetica" :height 180
				 :weight light))))
   '(fixed-pitch ((t ( :family "Fira Code" :height 140)))))
  (custom-theme-set-faces                                                       
   'user
   '(org-block ((t (:inherit fixed-pitch))))                                   
   '(org-code ((t (:family "Fira Code"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))          
   '(org-meta-line ((t (:inherit (
				  fixed-pitch)
				 :height 100
				 ))))
   '(org-block-begin-line ((t (:inherit (org-meta-line) ;; :foreground "#cccccc"
					:weight light))))
     
   '(org-property-value ((t (:inherit fixed-pitch))) t)                        
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-table ((t (:family "Fira Code" :foreground "#83a598"))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  
  ;; Use variable pitch mode
  (add-hook 'org-mode-hook (lambda ()
			     (variable-pitch-mode 1)
			     (setq-local line-spacing 1.0)
			     (set-fringe-mode 0)
			     (visual-line-mode 1)
			     (set-window-margins nil 3 3)
			     ))
  )

;; (use-package flymake-grammarly
;;   :config
;;   (add-hook 'org-mode-hook 'flymake-grammarly-load)
;;   (setq flymake-grammarly-check-time 0.8)
;;   )

(use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :defer t  ; defer package loading
  :hook ((org-mode markdown-mode). (lambda ()
                                      (require 'eglot-grammarly)
                                      (eglot-ensure))))

(use-package eglot
  :straight nil
  :bind (("M-c" . nil)
	 ("M-c ." . eglot-code-action-quickfix))
  :config
  (setq-default eglot-workspace-configuration
		'((:gopls . (:linksInHover :json-false
					   :completeUnimported  t))
		  (:grammarly . (:config . ((documentDialect . ("british")))))
		  ;; (:documentDialect . "british" ;;grammarly-languageserver
		  ;;  ;;:grammarly-languageserver ;; :@emacs-grammarly/grammarly-languageserver
		  ;;  ;;. ( ;;:grammarly.config.suggestions.PassiveVoice t
		  ;; 				     ;; (config.documentDialect . "british")
		  ;; 				     )
		  ))
  )


(use-package go-mode)

(use-package which-key
  :config
  (setq which-key-show-early-on-C-h t)
  (which-key-mode)
  )

(use-package yaml-mode)
