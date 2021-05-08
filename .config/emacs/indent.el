(setq-default tab-width 4)
(add-hook 'prog-mode-hook
		  (lambda ()
			  (setq tab-width 4
					lisp-body-indent 4
					python-indent-offset 4
					indent-tabs-mode t
					backward-delete-char-untabify-method nil)))
