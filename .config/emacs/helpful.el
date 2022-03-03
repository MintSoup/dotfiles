;;; -*- lexical-binding: t -*-
(use-package helpful
	:straight t
	:init
	(advice-add 'describe-function :override 'helpful-function)
	(advice-add 'describe-key :override 'helpful-key)
	(advice-add 'describe-variable :override 'helpful-variable)
	(advice-add 'describe-symbol :override 'helpful-symbol)
	:config
    (defvar read-symbol-positions-list nil)
    (defun helpful--autoloaded-p (sym buf)
		"Return non-nil if function SYM is autoloaded."
		(-when-let (file-name (buffer-file-name buf))
			(setq file-name (s-chop-suffix ".gz" file-name))
			(help-fns--autoloaded-p sym)))
    (defun helpful--skip-advice (docstring)
		"Remove mentions of advice from DOCSTRING."
		(let* ((lines (s-lines docstring))
               (relevant-lines
				(--take-while
				 (not (or (s-starts-with-p ":around advice:" it)
                          (s-starts-with-p "This function has :around advice:" it)))
				 lines)))
			(s-trim (s-join "\n" relevant-lines)))))
