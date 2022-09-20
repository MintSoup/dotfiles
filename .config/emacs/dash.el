;;; -*- lexical-binding: t -*-

;;(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

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
		dashboard-page-separator ""
		dashboard-set-init-info nil
		dashboard-footer-icon #("" 0 1
								(face
								 (:family "file-icons" :height 1.32 :inherit font-lock-keyword-face)
								 font-lock-face
								 (:family "file-icons" :height 1.32 :inherit font-lock-keyword-face)
								 display
								 (raise -0.06)
								 rear-nonsticky t))
		dashboard-footer-messages (list "The one true editor, Emacs!"
										"Who the hell uses VIM anyway? Go Evil!"
										"Free as free speech, free as free Beer"
										"Happy coding!"
										"Vi Vi Vi, the editor of the beast"
										"Welcome to the church of Emacs"
										"While any text editor can save your files, only Emacs can save your soul"
										"I showed you my source code, pls respond"
										"Richard Stallman is proud of you"))
  (defun set-dashboard-face (frame)
	(set-face-attribute 'dashboard-banner-logo-title nil
						:inherit 'variable-pitch
						:foreground (doom-color 'blue)
						:height 160))
  (mapc 'set-dashboard-face (frame-list))
  (add-hook 'after-make-frame-functions 'set-dashboard-face)
  (add-hook 'before-make-frame-hook 'dashboard-init-hack)
  (dashboard-setup-startup-hook))
