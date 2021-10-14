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
	"Load file from `user-emacs-directory'"
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
(load-user-config-file "tree.el")
(load-user-config-file "margin.el")
(load-user-config-file "lsp.el")
(load-user-config-file "magit.el")
(load-user-config-file "gud.el")
(load-user-config-file "sr-speedbar.el")
(load-user-config-file "torrent.el")
(load-user-config-file "gnus.el")
(load-user-config-file "as.el")

;; Lang
(load-user-config-file "geiser.el") ;; Scheme
(load-user-config-file "lua.el") ;; Lua
(load-user-config-file "carbon.el") ;; Carbon
(load-user-config-file "stm32.el") ;; STM32

(setq gc-cons-threshold (* 150 1024 1024))

(defun risky-local-variable-p (sym &optional _ignored) nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((+run-function run-in-vterm "./bfc bruh.b" 'projectile-run-vterm)
	 (+run-function run-in-vterm "./bfc" 'projectile-run-vterm)
	 (+debug-function stm32-start-gdb)
	 (+run-function run-in-vterm "./carbon test.cbn" projectile-run-vterm)
	 (+run-function run-in-vterm "./carbon test.cbn" 'projectile-run-vterm)
	 (+debug-function projectile-run-gdb)
	 (+debug-function . projectile-start-gdb)
	 (projectile-project-debug-cmd . "make -j12")
	 (projectile-project-debug-cmd . "make -j12 debug")
	 (projectile-project-compilation-cmd . "make -j12")
	 (+debug-function . projectile-run-gdb)
	 (+debug-function . stm32-start-gdb))))
