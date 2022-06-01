;;; -*- lexical-binding: t -*-
(defun dashboard-init-hack ()
	(setq initial-buffer-choice
		  (lambda ()
			  (get-buffer "*dashboard*")))
	(remove-hook 'before-make-frame-hook 'dashboard-init-hack))
(use-package dashboard
	:straight t
	:after doom-modeline
	:config
 	(setq inhibit-startup-screen t
 		  dashboard-center-content t
 		  dashboard-show-shortcuts nil
 		  dashboard-items nil
 		  dashboard-banner-logo-title "Welcome back to GNU Emacs"
 		  dashboard-startup-banner (expand-file-name "emacs.svg" user-emacs-directory)
 		  ;; dashboard-footer-messages nil
 		  dashboard-page-separator ""
 		  dashboard-set-init-info nil)
	(defun set-dashboard-face (frame)
		(set-face-attribute 'dashboard-banner-logo-title nil
							:inherit 'variable-pitch
							:foreground (doom-color 'blue)
							:height 160))
	(mapc 'set-dashboard-face (frame-list))
	(add-hook 'after-make-frame-functions 'set-dashboard-face)
	(add-hook 'before-make-frame-hook 'dashboard-init-hack)
	(dashboard-setup-startup-hook))
