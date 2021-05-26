;;; -*- lexical-binding: t -*-
(use-package quickrun
	:straight t)

(defvar quickrun-mode-commands
	'((emacs-lisp-mode . eval-buffer)
	  (scheme-mode . geiser-eval-buffer)))

(defun +quickrun ()
	"Quickrun but better"
	(interactive)
	(let ((binding
		   (assoc
			major-mode
			quickrun-mode-commands)))
		(if binding
				(funcall (cdr binding))
			(quickrun))))
