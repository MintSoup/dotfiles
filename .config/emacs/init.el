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
(load-user-config-file "stm32.el") ;; STM32

(setq gc-cons-threshold (* 150 1024 1024))

(defun risky-local-variable-p (sym &optional _ignored) nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3f1a4818bdd754dfe0747002b83f7a0965a1fda17a9cc4cddc285011c9c34645" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "ea20874dcf41694cd2e2ca402038960ffa93a164de795a0e56fc72fb35bf8c8f" "9417799814ac7faca7549db8bfdd5ce4db393cc6137021bb8557c1d05acfdb71" "771dfb93c33da6fe83fbdc24286d79560ecd370f241d231e879ae32154e05cc9" "db3fbcb30643539a048948cc88d2c556cd52809ad2d38783455b3241f32bdeed" "a268a000c46c4a1e14011815e804120ccc435aa6729cec8a64abc95ca7c78e6a" "fa240758b52073f8ae9e32840d833554ea70724142d23b2f6d32c602450b690f" "149f32ee640968581761f26674708a40f4156502a0d5027901e57c01fb96af42" "fea4d259e500e937b14491a646d87e2b756fe7f883d2a1493ce483c013c318a4" default))
 '(safe-local-variable-values
   '((+run-function run-in-vterm "./carbon test.cbn" projectile-run-vterm)
	 (+run-function run-in-vterm "./carbon test.cbn" 'projectile-run-vterm)
	 (+debug-function projectile-run-gdb)
	 (+debug-function . projectile-start-gdb)
	 (projectile-project-debug-cmd . "make -j12")
	 (projectile-project-debug-cmd . "make -j12 debug")
	 (projectile-project-compilation-cmd . "make -j12")
	 (+debug-function . projectile-run-gdb)
	 (+debug-function . stm32-start-gdb))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
