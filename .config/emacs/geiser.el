;;; -*- lexical-binding: t -*-
(use-package geiser
	:straight t)

(use-package geiser-guile
	:straight t
	:after company)
(defun guile-run-external ()
	(interactive)
	(let ((file-path (make-temp-file "guile-geiser")))
		(delete-file file-path)
		(start-process "guile repl"
					   " *Guile External Repl*"
					   "guile"
					   (format "--listen=%s" file-path))
		(sleep-for 0.05) ;; Wait for the process to start
		(geiser-connect-local (geiser-repl--get-impl
							  "Connect to Scheme implementation: ")
							  file-path)))
