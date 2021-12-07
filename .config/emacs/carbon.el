(let* ((kwds '("and"
			   "or"
			   "is"
			   "as"
			   "return"
			   "end"
			   "while"
			   "break"
			   "continue"
			   "for"
			   "in"
			   "if"
			   "else"
			   "elif"
			   "class"
			   "super"
			   "import"))
	   (type '("bool"
			   "uint"
			   "int"
			   "double"
			   "string"
			   "array"
			   "generator"
			   "table"
			   "function"
			   "void"
			   "object"))
	   (cnst '("false"
			   "true"
			   "null"))
	   (kwdsr (regexp-opt kwds 'words))
	   (typer (regexp-opt type 'words))
	   (cnstr (regexp-opt cnst 'words)))
	(setq carbon-font-lock-highlights
		  `(("'.*'" . font-lock-string-face)
			(,kwdsr . font-lock-keyword-face)
			("print" . font-lock-builtin-face)
			(,typer . font-lock-type-face)
			(,cnstr . font-lock-constant-face)
			("@[a-zA-Z1-9]+" . font-lock-type-face))))


(define-derived-mode carbon-mode prog-mode "Carbon"
	"Major mode for editing Carbon source code"
	(setq-local font-lock-defaults '(carbon-font-lock-highlights)
				comment-start "# "
				comment-start-skip "#+\\s-*"))

(add-to-list 'auto-mode-alist '("\\.cbn\\'" . carbon-mode))
