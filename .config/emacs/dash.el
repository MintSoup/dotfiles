;;; -*- lexical-binding: t -*-
(use-package dashboard
	:straight t
	:after doom-modeline
	:init
	(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
		  dashboard-center-content t
		  dashboard-show-shortcuts nil
		  dashboard-items nil
		  dashboard-banner-logo-title "Welcome back to GNU Emacs"
		  dashboard-startup-banner (expand-file-name "emacs.svg" user-emacs-directory)
		  dashboard-footer-messages nil
		  dashboard-page-separator ""
		  dashboard-set-init-info nil)
	:config
	(defun set-dashboard-face (frame)
		(set-face-attribute 'dashboard-banner-logo-title nil
							:font "Noto Sans"
							:foreground "#51afef"
							:height 160))
	(mapc 'set-dashboard-face (frame-list))
	(add-hook 'after-make-frame-functions 'set-dashboard-face)
	(dashboard-setup-startup-hook))
