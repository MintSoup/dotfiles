;;; -*- lexical-binding: t -*-

(defun project-current-root ()
  (project-root (project-current)))

(defmacro project-with-default-dir (&rest body)
  `(let ((default-directory (project-current-root)))
	 ,@body))

(defun +project-prefixed-buffer-name (mode)
  (concat "*" (downcase mode) " " (project-name (project-current)) "*"))

(advice-add 'project-prefixed-buffer-name :override '+project-prefixed-buffer-name)

(defun project-vterm (&optional ARG)
  (interactive "P")
  (project-with-default-dir
   (let ((vterm-buffer-name (project-prefixed-buffer-name "vterm")))
	 (vterm-other-window ARG))))

(defun project-try-directory (dir)
  (cons 'transient dir))

(setq project-switch-commands
	  '((project-find-file "Find file")
		((lambda ()
		   (interactive)
		   (project-with-default-dir
			(magit-status))) "Magit" "g")
		(project-eshell "Eshell")
		(project-vterm "VTerm" "v"))
	  project-find-functions
	  '(project-try-vc
		project-try-directory))

(defvar debug-command "make -j12"
  "Command used for compiling project in debug mode.")

(defun +project-debug ()
  (interactive)
  (when (boundp '+debug-function)
	(let* ((fn +debug-function)
		   (compile-command debug-command)
		   (compilation-buffer (project-compile)))
	  (with-current-buffer compilation-buffer
		(setq +on-finish-compilation-function fn)))))

(defvar-local +on-finish-compilation-function nil)

(defun +project-run ()
  (interactive)
  (when (boundp '+run-function)
	;; NOTE: we must bind fn *before* compilation-buffer
	;; as the popup compilation buffer automatically gets selected and overwrites
	;; the current buffer.
	(let* ((fn +run-function)
		   (compilation-buffer (project-compile)))
	  (with-current-buffer compilation-buffer
		(setq-local +on-finish-compilation-function fn)))))

(defun +post-compile (buffer status)
  (when (and
		 (string-equal status "finished\n")
		 +on-finish-compilation-function)
	(funcall +on-finish-compilation-function)))

(add-hook 'compilation-finish-functions '+post-compile)

(use-package project-rootfile
  :straight t
  :config
  ;; To get the ordering right
  (setq project-find-functions
		'(project-rootfile-try-detect project-try-vc project-try-directory)))
