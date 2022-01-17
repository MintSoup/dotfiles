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

(defalias 'counsel-projectile-find-file-transformer 'identity) ;; Otherwise it's slow af

(use-package counsel-projectile
	:straight t
	:config
	(counsel-projectile-mode))

(add-hook 'projectile-after-switch-project-hook
		  (lambda ()
			  (projectile-invalidate-cache nil)))
