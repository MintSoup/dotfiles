(use-package projectile
	:straight t
	:init
	(setq projectile-enable-caching t
		  projectile-indexing-method 'alien
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
			 (or (string-suffix-p "evil-ex.el" file) (cl-some
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


(use-package counsel-projectile
	:straight t
	:config
	(counsel-projectile-mode)
	:init
	(setq counsel-projectile-find-file-matcher 'ivy--re-filter))
