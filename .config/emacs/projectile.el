(use-package projectile
	:straight t
	:init
	(setq projectile-enable-caching t
		  projectile-require-project-root nil)
	:config
	(projectile-mode +1))

(use-package counsel-projectile
	:straight t)
