;;; -*- lexical-binding: t -*-
(use-package geiser
	:straight t
	:init
	(setq geiser-debug-jump-to-debug-p nil)
	:config
	(my-local-leader :keymaps 'geiser-mode-map
		"d"  '(:wk "Documentation")
		"dd" '(geiser-doc-symbol-at-point :wk "Doc for symbol at point")
		"dm" '(geiser-doc-module :wk "Doc for module")
		"di" '(counsel-geiser-doc-look-up-manual :wk "Info for symbol")))

(use-package geiser-guile
	:straight t
	:after company)

(defun guile-run-external ()
	(interactive)
	(let ((file-path (make-temp-file "guile-geiser")))
		(delete-file file-path)
		(start-process "guile repl"
					   " *Guile External REPL*"
					   "guile" "--listen")
		(sleep-for 0.05) ;; Wait for the process to start
		(geiser-connect 'guile "localhost" 37146)))
