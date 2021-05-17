(use-package quickrun
	:straight t)

(defun +quickrun ()
	"Quickrun but better"
	(interactive)
	(if (eq major-mode 'emacs-lisp-mode)
 		(eval-buffer)
		(quickrun)))
