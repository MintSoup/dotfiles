;;; -*- lexical-binding: t -*-
(use-package treemacs
	:straight t
	:after doom-themes
	:config
	(setq doom-themes-treemacs-enable-variable-pitch t
		  doom-themes-treemacs-theme "doom-colors"
		  treemacs-width 30)
	(doom-themes-treemacs-config)
	;; dirty trick to fix project-follow-mode
	(load-user-config-file
	 "straight/repos/treemacs/src/elisp/treemacs-project-follow-mode.el")
	(treemacs-project-follow-mode))

(use-package treemacs-projectile
	:straight t)

(use-package treemacs-evil
	:straight t)
