;;; -*- lexical-binding: t -*-
(use-package visual-fill-column
	:straight t
	:init
	 (setq visual-fill-column-width 100
           visual-fill-column-center-text t))

(defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))
