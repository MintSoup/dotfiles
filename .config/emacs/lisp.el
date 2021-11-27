;;; -*- lexical-binding: t -*-

(use-package evil-cleverparens
	:straight t
	:hook (emacs-lisp-mode . evil-cleverparens-mode)
	:hook (scheme-mode . evil-cleverparens-mode))

(general-define-key
 :keymaps 'evil-cleverparens-mode-map
 :states 'normal
 "s" 'evil-snipe-s)
