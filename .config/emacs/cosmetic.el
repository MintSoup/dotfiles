;;; -*- lexical-binding: t -*-
(use-package doom-themes
	:straight t
	:config
	(setq doom-themes-enable-bold t
		  doom-themes-enable-italic t)
	(doom-themes-org-config)
	(load-theme 'my-dark t))

(use-package doom-modeline
	:straight t
	:init
	(setq doom-modeline-major-mode-icon t
		  doom-modeline-icon t
		  doom-modeline-height 28
		  doom-modeline-buffer-file-name-style 'relative-from-project
		  projectile-dynamic-mode-line nil)
 	(column-number-mode +1)
	(doom-modeline-mode +1))

(use-package highlight-quoted
	:straight t
	:config
	(add-hook 'lisp-mode-hook 'highlight-quoted-mode)
	(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package rainbow-delimiters
	:straight t
	:config
	(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-numbers
	:straight t
	:config
	(add-hook 'prog-mode-hook 'highlight-numbers-mode))


(defun my-setup-fonts (&optional fonts)
	(set-face-attribute 'fixed-pitch nil
						:font "Fira Code Nerd Font"
						:inherit 'default
						:height 0.95)

	(set-face-attribute 'default nil
						:font "Fira Code Nerd Font"
						:height 120
						:weight 'medium)

	(set-face-attribute 'variable-pitch nil
						:font "Cantarell"
						:height 136
						:weight 'medium)

	;; (set-fontset-font "-CTDB-FiraCode Nerd Font-normal-normal-normal-*-20-*-*-*-d-0-fontset-auto2" 'armenian (font-spec :family "DejaVu Sans Code"))
	;; Makes commented text and keywords italics.
	;; This is working in emacsclient but not emacs.
	;; Your font must have an italic face available.
	(set-face-attribute 'font-lock-comment-face nil
						:slant 'italic)
	(set-face-attribute 'font-lock-keyword-face nil
						:slant 'italic))

(my-setup-fonts)

(add-hook 'after-make-frame-functions 'my-setup-fonts)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
