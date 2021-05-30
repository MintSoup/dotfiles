;;; -*- lexical-binding: t -*-

(electric-pair-mode +1)

(setq recentf-max-menu-items 100
	  recentf-max-saved-items 100)
(recentf-mode +1)
(global-visual-line-mode +1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

(defun my-prog-nuke-trailing-whitespace ()
	(when (derived-mode-p 'prog-mode)
		(delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)
