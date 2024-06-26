(require 'treesit)
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")

	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((emacs-lisp-mode . )
	(c++-mode . c++-ts-mode)
	(bash-mode . bash-ts-mode)
	(js2-mode . js-ts-mode)
	(typescript-mode . typescript-ts-mode)
	(json-mode . json-ts-mode)
	(css-mode . css-ts-mode)
	(python-mode . python-ts-mode)))

(provide 'my-treesit)
