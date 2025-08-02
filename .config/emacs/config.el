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


(add-hook 'imenu-after-jump-hook
		  (lambda ()
			(evil-scroll-line-to-top (line-number-at-pos))))

(use-package lorem-ipsum
  :straight t)

(use-package fish-mode
  :straight t)

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


(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"
		;; To allow logging into non-standard
		;; systems, i.e. guix or nix where
		;; /bin/ls doesn't exist
		tramp-remote-path (append tramp-remote-path
								  '(tramp-own-remote-path))))

(setq sentence-end-double-space nil)


(use-package ztree
  :straight t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
	  ediff-split-window-function 'split-window-horizontally)

(setq epg-pinentry-mode 'loopback)

(setq gamegrid-glyph-height-mm 7.0)

(setq enable-recursive-minibuffers t)

(setq comint-input-ignoredups t)

(setq compilation-scroll-output t)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


(use-package ace-window
  :straight t
  :config
  (ace-window-display-mode))

(use-package wgrep
  :straight t
  :config
  (general-define-key :keymaps 'grep-mode
					  :states 'normal
					  "i" 'wgrep-change-to-wgrep-mode)
  (general-define-key :keymaps 'wgrep-mode-map
					  :states 'normal
					  "i" 'evil-cp-insert))

(general-define-key
 :keymaps '(c-mode-map c++-mode-map java-mode-map)
 :states '(normal insert)
 "C-<return>" 'c-indent-new-comment-line)
