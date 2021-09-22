;;; -*- lexical-binding: t -*-
(use-package treemacs
	:straight t
	:config
	(setq doom-themes-treemacs-enable-variable-pitch t
		  doom-themes-treemacs-theme "doom-colors")
	(doom-themes-treemacs-config))

(use-package treemacs-projectile
	:straight t)
