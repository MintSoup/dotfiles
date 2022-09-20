;;; -*- lexical-binding: t -*-
(use-package quickrun
  :straight t)

(defvar quickrun-mode-commands
  '((emacs-lisp-mode . eval-buffer)
	(scheme-mode . geiser-eval-buffer)
	(lisp-mode . sly-eval-buffer)))

(defun +quickrun ()
  "Quickrun but better"
  (interactive)
  (-if-let (binding (assoc major-mode quickrun-mode-commands))
	  (funcall (cdr binding))
	(quickrun)))
