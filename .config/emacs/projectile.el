(use-package projectile
	:straight t
	:init
	(setq projectile-enable-caching t
		  projectile-indexing-method 'hybrid
		  projectile-require-project-root nil
		  projectile-sort-order 'recently-active)
	:config
	(defadvice projectile-project-root (around ignore-remote first activate)
		(unless (file-remote-p default-directory) ad-do-it))
	(projectile-mode +1))

(use-package counsel-projectile
	:straight t
	:config
	(counsel-projectile-mode)
	:init
	(setq counsel-projectile-find-file-matcher 'ivy--re-filter))
