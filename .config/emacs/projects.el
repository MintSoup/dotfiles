(use-package projectile
	:straight t
	:init
	(setq projectile-enable-caching t
		  projectile-indexing-method 'hybrid
		  projectile-require-project-root nil
		  projectile-sort-order 'recently-active
		  projectile-globally-ignored-directories '())
	:config
	(defadvice projectile-project-root (around ignore-remote first activate)
		(unless (file-remote-p default-directory) ad-do-it))
	(projectile-mode +1))

(defun projectile-remove-ignored (files)
	"Remove ignored files and folders from FILES.

If ignored directory prefixed with '*', then ignore all
directories/subdirectories with matching filename,
otherwise operates relative to project root."
	(let ((ignored-files (projectile-ignored-files-rel))
          (ignored-dirs (projectile-ignored-directories-rel))
		  (root (projectile-project-root)))
		(cl-remove-if
		 (lambda (file)
			 (if (string-prefix-p "./" file)
					 (setq file (string-trim-left file "\\./")))
			 (or (cl-some
				  (lambda (f)
					  (string= f (file-name-nondirectory file)))
				  ignored-files)
				 (cl-some
				  (lambda (dir)
					  ;; if the directory is prefixed with '*' then ignore all directories matching that name
					  (if (string-prefix-p "*" dir)
							  ;; remove '*' and trailing slash from ignored directory name
							  (let ((d (substring dir 1 (if (equal (substring dir -1) "/") -1 nil))))
								  (cl-some
								   (lambda (p)
									   (string= d p))
								   ;; split path by '/', remove empty strings, and check if any subdirs match name 'd'
								   (delete "" (split-string (or (file-name-directory file) "") "/"))))
						  (string-prefix-p dir file)))
				  ignored-dirs)
				 (cl-some
				  (lambda (suf)
					  (string-suffix-p suf file t))
				  projectile-globally-ignored-file-suffixes)))
		 files)))


(cl-defun projectile--run-project-cmd
		(command command-map &key show-prompt prompt-prefix save-buffers use-comint-mode)
	"Run a project COMMAND, typically a test- or compile command.

Cache the COMMAND for later use inside the hash-table COMMAND-MAP.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
by setting SHOW-PROMPT.  The prompt will be prefixed with PROMPT-PREFIX.

If SAVE-BUFFERS is non-nil save all projectile buffers before
running the command.
The compilation buffer is returned
"
	(let* ((project-root (projectile-project-root))
           (default-directory (projectile-compilation-dir))
           (command (projectile-maybe-read-command show-prompt
                                                   command
                                                   prompt-prefix))
           compilation-buffer-name-function
           compilation-save-buffers-predicate)
		(when command-map
			(puthash default-directory command command-map)
			(ring-insert (projectile--get-command-history project-root) command))
		(when save-buffers
			(save-some-buffers (not compilation-ask-about-save)
							   (lambda ()
								   (projectile-project-buffer-p (current-buffer)
																project-root))))
		(when projectile-per-project-compilation-buffer
			(setq compilation-buffer-name-function #'projectile-compilation-buffer-name)
			(setq compilation-save-buffers-predicate #'projectile-current-project-buffer-p))
		(unless (file-directory-p default-directory)
			(mkdir default-directory))
		(projectile-run-compilation command use-comint-mode)))


(defun +project-debug ()
	(interactive)
	(when (boundp '+debug-form)
		(let ((compilation-buffer
			   (projectile--run-project-cmd projectile-project-debug-cmd projectile-compilation-cmd-map
											:prompt-prefix "Compile command: "
											:save-buffers t
											:use-comint-mode projectile-compile-use-comint-mode))
			  (form +debug-form))
			(with-current-buffer compilation-buffer
				(setq-local +on-finish-compilation-form form)))))

(defun +project-run ()
	(interactive)
	(when (boundp '+run-form)
		(let ((compilation-buffer
			   (projectile--run-project-cmd projectile-project-compilation-cmd projectile-compilation-cmd-map
											:prompt-prefix "Compile command: "
											:save-buffers t
											:use-comint-mode projectile-compile-use-comint-mode))
			  (form +run-form))
			(with-current-buffer compilation-buffer
				(setq-local +on-finish-compilation-form form)))))

(defun +post-compile (buffer status)
	(when (and
		   (string-equal status "finished\n")
		   (buffer-local-boundp '+on-finish-compilation-form buffer))
		(eval (buffer-local-value
			   '+on-finish-compilation-form buffer))))

(add-hook 'compilation-finish-functions '+post-compile)

(use-package counsel-projectile
	:straight t
	:config
	(counsel-projectile-mode)
	:init
	(setq counsel-projectile-find-file-matcher 'ivy--re-filter))
