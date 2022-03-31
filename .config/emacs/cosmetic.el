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
	:config
	(setq doom-modeline-height 28
		  doom-modeline-buffer-file-name-style 'relative-from-project)
	(doom-modeline-def-segment window-number
		"The current window number."
		(let ((num (cond
					((bound-and-true-p ace-window-display-mode)
					 (aw-update)
					 (window-parameter (selected-window) 'ace-window-path))
					((bound-and-true-p winum-mode)
					 (setq winum-auto-setup-mode-line nil)
					 (winum-get-number-string))
					((bound-and-true-p window-numbering-mode)
					 (window-numbering-get-number-string))
					(t ""))))
			(if (and (< 0 (length num))
					 (< 1 (length (cl-mapcan
								   (lambda (frame)
									   ;; Exclude minibuffer and child frames
									   (unless (and (fboundp 'frame-parent)
													(frame-parent frame))
										   (window-list frame 'never)))
								   (visible-frame-list)))))
					(propertize (format " %s" num)
								'face (if (doom-modeline--active)
											  'doom-modeline-buffer-major-mode
										  'mode-line-inactive)))))
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
	(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-numbers
	:straight t
	:config
	(add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package rainbow-mode
	:straight t)

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
						:font "Noto Sans"
						:weight 'regular
						:height 125)

	(set-fontset-font t 'armenian "Noto Sans Armenian"))

(my-setup-fonts)

(add-hook 'after-make-frame-functions 'my-setup-fonts)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq x-stretch-cursor t)
