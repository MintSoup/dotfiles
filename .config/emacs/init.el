;;; -*- lexical-binding: t -*-
;;; Straight.el init

;; Straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun load-user-config-file (name)
  "Load file from `user-emacs-directory'"
  (load-file (expand-file-name name user-emacs-directory)))

;; Core
(load-user-config-file "general.el")
(load-user-config-file "evil.el")
(load-user-config-file "config.el")
;; (load-user-config-file "ivy.el")
(load-user-config-file "completion.el")

;; Less important
(load-user-config-file "scroll.el")
(load-user-config-file "indent.el")

;; Theme
(load-user-config-file "cosmetic.el")

;; Aux
(load-user-config-file "helpful.el")
(load-user-config-file "buffers.el")
(load-user-config-file "projects.el")
(load-user-config-file "dired.el")
(load-user-config-file "popups.el")
(load-user-config-file "dash.el")
(load-user-config-file "format.el")
(load-user-config-file "org.el")
(load-user-config-file "vterm.el")
(load-user-config-file "whichkey.el")
(load-user-config-file "company.el")
(load-user-config-file "lsp.el")
(load-user-config-file "quickrun.el")
(load-user-config-file "erc.el")
(load-user-config-file "margin.el")
(load-user-config-file "magit.el")
(load-user-config-file "gud.el")
(load-user-config-file "torrent.el")
(load-user-config-file "mail.el")
(load-user-config-file "eshell.el")
(load-user-config-file "ligatures.el")
(load-user-config-file "snippets.el")
(load-user-config-file "restclient.el")
(load-user-config-file "flycheck.el")
;; (load-user-config-file "beardbolt.el")

;; Lang
(load-user-config-file "geiser.el") ;; Scheme
(load-user-config-file "lua.el")
(load-user-config-file "carbon.el")
(load-user-config-file "stm32.el")
(load-user-config-file "as.el")
(load-user-config-file "lisp.el")
(load-user-config-file "web.el")
(load-user-config-file "cmake.el")
(load-user-config-file "typst.el")


;; Other
(load-user-config-file "private.el")

(setq gc-cons-threshold (* 50 1024 1024))

(defun risky-local-variable-p (sym &optional _ignored) nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8714a49a666c210d7527fe6cf325d6cba5432ccc46a7afd2943a21a3d8d0febc"
	 "97ca1d334d8e728ee42aef7bdcd15fe5539cc88e9642773cea1758dbaf0c22c4"
	 default))
 '(safe-local-variable-values
   '((+debug-function . stm32-start-gdb) (debug-command . "make -j12")
	 (+debug-function lambda nil
					  (project-with-default-dir
					   (call-interactively #'gdb)))
	 (+debug-function lambda nil (project-with-default-dir (gdb)))
	 (projectile-project-debug-cmd . "make -j12")
	 (projectile-project-compilation-cmd . "make -j12")
	 (+run-function lambda nil
					(project-with-default-dir
					 (run-in-vterm "./carbon test.cbn" #'vterm)))
	 (+debug-function . projectile-run-gdb)
	 (debug-command . "make -j12 debug")))
 '(warning-suppress-log-types '((native-compiler))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
