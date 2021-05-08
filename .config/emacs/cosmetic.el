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
		  doom-modeline-buffer-file-name-style 'relative-to-project
		  projectile-dynamic-mode-line nil)
	(setq mode-line-format nil)
	(doom-modeline-mode 1))

(use-package highlight-quoted
	:straight t
	:config
	(add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package rainbow-delimiters
	:straight t
	:config
	(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-numbers
	:straight t
	:config
	(add-hook 'prog-mode-hook 'highlight-numbers-mode))



(set-face-attribute 'default nil
					:font "Fira Code Nerd Font"
					:height 100
					:weight 'medium)

(set-face-attribute 'variable-pitch nil
					:font "Noto Sans"
					:height 150
					:weight 'medium)

(set-face-attribute 'fixed-pitch nil
					:font "Fira Code Nerd Font"
					:height 150
					:weight 'medium)

;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
					:slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
					:slant 'italic)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Fira Code Nerd Font-14"))
;; changes certain keywords to symbols, such as lamda!
;; (setq global-prettify-symbols-mode t)



(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)