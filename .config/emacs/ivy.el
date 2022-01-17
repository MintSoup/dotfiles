;;; -*- lexical-binding: t -*-
(use-package ivy
	:straight t
	:defer 0.1
	:diminish
	:init
	(setq ivy-height 15
		  ivy-initial-inputs-alist nil
		  ivy-on-del-error-function #'ignore
		  ivy-re-builders-alist '((t . ivy--regex-ignore-order))
		  enable-recursive-minibuffers t)
	:config
	(general-define-key
	 :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
	 "C-k" 'ivy-previous-line
	 "C-j" 'ivy-next-line
	 "C-h" 'evil-backward-char
	 "C-l" 'evil-forward-char
	 "C-w" 'ivy-backward-kill-word)

	(general-define-key
	 :keymaps 'minibuffer-mode-map
	 "C-h" 'evil-backward-char
	 "C-l" 'evil-forward-char
	 "C-w" 'ivy-backward-kill-word)

	(ivy-mode))

(defun +counsel-switch-buffer-hide-asterisk ()
	(interactive)
	(let ((ivy-ignore-buffers '("\\` " "\\`\\*")))
		(counsel-switch-buffer)))

(use-package counsel
	:straight t
	:after ivy
	:config (counsel-mode +1))

(use-package counsel-fd
	:straight t
	:after counsel)

(use-package all-the-icons-ivy-rich
	:straight t
	:after counsel-projectile
	:init
	(setq all-the-icons-ivy-rich-project nil)
	:config
	(all-the-icons-ivy-rich-mode 1)
	(ivy-rich-mode 1))

(use-package ivy-rich
	:straight t
	:after all-the-icons-ivy-rich
	:init
	(setq ivy-rich-path-style 'abbrev
          ivy-virtual-abbreviate 'full))

(use-package swiper
	:straight t
	:after ivy)

(with-eval-after-load 'ivy-rich
	(defvar ek/ivy-rich-cache
		(make-hash-table :test 'equal))

	(defun ek/ivy-rich-cache-lookup (delegate candidate)
		(let ((result (gethash candidate ek/ivy-rich-cache)))
			(unless result
				(setq result (funcall delegate candidate))
				(puthash candidate result ek/ivy-rich-cache))
			result))

	(defun ek/ivy-rich-cache-reset ()
		(clrhash ek/ivy-rich-cache))

	(defun ek/ivy-rich-cache-rebuild ()
		(mapc (lambda (buffer)
				  (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
			  (buffer-list)))

	(defun ek/ivy-rich-cache-rebuild-trigger ()
		(ek/ivy-rich-cache-reset)
		(run-with-idle-timer 1 nil 'ek/ivy-rich-cache-rebuild)))
