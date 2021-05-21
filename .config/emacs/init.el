;;; -*- lexical-binding: t -*-
;;; Straight.el init

;; (defvaralias 'comp-deferred-compilation-deny-list 'native-comp-deferred-compilation-deny-list)


(defvar bootstrap-version)

(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
	(unless (file-exists-p bootstrap-file)
		(with-current-buffer
				(url-retrieve-synchronously
				 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
				 'silent 'inhibit-cookies)
			(goto-char (point-max))
			(eval-print-last-sexp)))
	(load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-clone-depth 1)


(straight-use-package 'use-package)

(defun load-user-config-file (name)
	(load-file (expand-file-name name user-emacs-directory)))



;; Core
(load-user-config-file "general.el")
(load-user-config-file "evil.el")
(load-user-config-file "config.el")
(load-user-config-file "ivy.el")
(load-user-config-file "scroll.el")
(load-user-config-file "indent.el")

;; Theme
(load-user-config-file "cosmetic.el")

;; Aux
(load-user-config-file "helpful.el")
(load-user-config-file "projectile.el")
(load-user-config-file "dired.el")
(load-user-config-file "popups.el")
(load-user-config-file "dash.el")
(load-user-config-file "format.el")
(load-user-config-file "org.el")
(load-user-config-file "vterm.el")
(load-user-config-file "whichkey.el")
(load-user-config-file "company.el")
(load-user-config-file "buffers.el")
(load-user-config-file "quickrun.el")
(load-user-config-file "erc.el")
(load-user-config-file "neotree.el")

(setq gc-cons-threshold (* 50 1024 1024))
