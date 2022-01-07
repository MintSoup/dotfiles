(use-package projectile
	:straight t
	:init
	(setq projectile-enable-caching t
		  projectile-indexing-method 'hybrid
		  projectile-require-project-root nil
		  projectile-sort-order 'recently-active)
	:config
	(projectile-mode +1))

(add-hook 'projectile-after-switch-project-hook (lambda ()
      (projectile-invalidate-cache nil)))

(use-package counsel-projectile
	:straight t)
