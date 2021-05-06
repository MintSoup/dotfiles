;;; -*- lexical-binding: t -*-
(use-package which-key
	:straight t
	:init
	(setq-default which-key-idle-delay 0.35
				  which-key-idle-secondary-delay 0.000001)
	:config
	(which-key-mode +1))

(electric-pair-mode +1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)) ;; one line at a time
	  mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
	  mouse-wheel-follow-mouse 't ;; scroll window under mouse
	  scroll-step 1 ;; keyboard scroll one line at a time
	  scroll-conservatively 10000
	  make-backup-files nil)

(setq enable-recursive-minibuffers t)

(setq my-skippable-buffer-regexp "^\*.+\*$")

(defun my-change-buffer (change-buffer)
	"Call CHANGE-BUFFER until `my-skippable-buffer-regexp' doesn't match."
	(let ((initial (current-buffer)))
		(funcall change-buffer)
		(let ((first-change (current-buffer)))
			(catch 'loop
				(while (string-match-p my-skippable-buffer-regexp (buffer-name))
					(funcall change-buffer)
					(when (eq (current-buffer) first-change)
						(switch-to-buffer initial)
						(throw 'loop t)))))))

(defun my-next-buffer ()
	"Variant of `next-buffer' that skips `my-skippable-buffer-regexp'."
	(interactive)
	(my-change-buffer 'next-buffer))

(defun my-previous-buffer ()
	"Variant of `previous-buffer' that skips `my-skippable-buffer-regexp'."
	(interactive)
	(my-change-buffer 'previous-buffer))



(setq-default tab-width 4)

(add-hook 'prog-mode-hook
		  (lambda ()
			  (setq tab-width 4
					lisp-body-indent 4
					python-indent-offset 4
					indent-tabs-mode t
					backward-delete-char-untabify-method nil)))

(setq org-superstar-leading-bullet "")
(global-visual-line-mode +1)
(recentf-mode +1)
(defun remove-scratch-buffer ()
	(if (get-buffer "*scratch*")
			(kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
