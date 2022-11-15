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
  (setq doom-modeline-height 32
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

(use-package hl-todo
  :straight t
  :init
  (setq hl-todo-highlight-punctuation ":"
		hl-todo-keyword-faces
		`(;; For things that need to be done, just not today.
		  ("TODO" warning bold)
		  ;; For problems that will become bigger problems later if not
		  ;; fixed ASAP.
		  ("FIXME" error bold)
		  ;; For tidbits that are unconventional and not intended uses of the
		  ;; constituent parts, and may break in a future update.
		  ("HACK" font-lock-constant-face bold)
		  ;; For things that were done hastily and/or hasn't been thoroughly
		  ;; tested. It may not even be necessary!
		  ("REVIEW" font-lock-keyword-face bold)
		  ;; For especially important gotchas with a given implementation,
		  ;; directed at another user other than the author.
		  ("NOTE" success bold)
		  ;; For things that just gotta go and will soon be gone.
		  ("DEPRECATED" font-lock-doc-face bold)
		  ;; For a known bug that needs a workaround
		  ("BUG" error bold)
		  ;; For warning about a problematic or misguiding code
		  ("XXX" font-lock-constant-face bold)))
  :config
  (global-hl-todo-mode))

(defun my-setup-fonts (&optional fonts)
  (set-face-attribute 'fixed-pitch nil
					  :font "Fira Code Nerd Font"
					  :inherit 'default
					  :height 0.95)

  (set-face-attribute 'default nil
					  :font "Fira Code Nerd Font"
					  :height 120
					  :weight 'normal)

  (set-face-attribute 'variable-pitch nil
					  :font "Noto Sans"
					  :weight 'normal
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
