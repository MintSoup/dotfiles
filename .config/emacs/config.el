;;; -*- lexical-binding: t -*-

(electric-pair-mode +1)

(setq recentf-max-menu-items 100
	  recentf-max-saved-items 100)

(recentf-mode +1)
(global-visual-line-mode +1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'Info-mode-hook 'visual-fill-column-mode)
(add-hook 'shell-mode-hook 'evil-normal-state)

(setq css-fontify-colors nil)

(defun my-prog-nuke-trailing-whitespace ()
	(when (derived-mode-p 'prog-mode)
		(delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

(setq make-backup-files nil)
(setq backup-directory-alist
	  `(("." . ,(concat user-emacs-directory "backups/"))))

(add-hook 'imenu-after-jump-hook '+scroll-current-line-to-top)

(defun +scroll-current-line-to-top ()
	(evil-scroll-line-to-top (line-number-at-pos)))

(use-package su
	:straight t
	:config
	(su-mode +1))

(winner-mode)

(defun indent-buffer ()
	"Call `indent-region' on the entire buffer."
	(interactive)
	(indent-region (point-min) (point-max) nil))

(defun tabify-buffer ()
	"Call `tabify' on the entire buffer."
	(interactive)
	(tabify (point-min) (point-max)))

(setq-default major-mode 'text-mode)

(setq tramp-default-method "ssh")

(setq sentence-end-double-space nil)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(general-define-key
 :keymaps 'override
 "C-M-u" 'universal-argument)
