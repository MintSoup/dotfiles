(setq-default tab-width 4)
(add-hook 'prog-mode-hook
		  (lambda ()
			  (setq tab-width 4
					lisp-body-indent 4
					lua-indent-level 4
					python-indent-offset 4
					c-basic-offset 4
					indent-tabs-mode t
					backward-delete-char-untabify-method nil
					sgml-basic-offset 4
					tab-always-indent nil)
			  (c-set-offset 'case-label '4)))
