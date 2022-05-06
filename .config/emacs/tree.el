;;; -*- lexical-binding: t -*-
(use-package treemacs
	:straight t
	:after doom-themes
	:config
	(setq doom-themes-treemacs-enable-variable-pitch t
		  doom-themes-treemacs-theme "doom-colors"
		  treemacs-read-string-input 'from-minibuffer
		  treemacs-width 30)
	(doom-themes-treemacs-config)
	(treemacs-project-follow-mode +1))

(use-package treemacs-projectile
	:straight t)

(use-package treemacs-evil
	:straight t)
