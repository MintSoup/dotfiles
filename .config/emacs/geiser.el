;;; -*- lexical-binding: t -*-
(use-package geiser
  :straight t
  :init
  (setq geiser-debug-jump-to-debug-p nil))

(my-local-leader :keymaps 'geiser-mode-map
  "d"  '(:wk "Documentation")
  "dd" '(+consult-geiser-doc-symbol :wk "Doc for symbol")
  "dm" '(geiser-doc-module :wk "Doc for module")
  ;; "di" '(counsel-geiser-doc-look-up-manual :wk "Info for symbol")
  )

(defun +consult-geiser-doc-symbol ()
  (interactive)
  (geiser-doc-symbol
   (intern
	(completing-read "Documentation: "
					 geiser-completion-symbol-list-func
					 #'always
					 t
					 (symbol-name (geiser--symbol-at-point))))))


(general-define-key :keymaps 'geiser-debug-mode-map
					:states 'normal
					"," 'geiser-debug--debugger-transient)

(use-package geiser-guile
  :straight t)

(defun guile-run-external ()
  (interactive)
  (let ((file-path (make-temp-file "guile-geiser")))
	(delete-file file-path)
	(start-process "guile repl"
				   " *Guile External REPL*"
				   "guile" "--listen")
	(sleep-for 0.05) ;; Wait for the process to start
	(geiser-connect 'guile "localhost" 37146)))

(defun mintywm ()
  (interactive)
  (geiser-connect-local 'guile "/tmp/mintywm"))

(add-hook 'scheme-mode-hook 'highlight-quoted-mode)
(add-hook 'geiser-mode-hook (lambda () (setq eldoc-idle-delay 0.0)))
