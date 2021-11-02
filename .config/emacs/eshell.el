;;; -*- lexical-binding: t -*-
;; (add-hook 'eshell-mode-hook
;; 		  (lambda ()
;; 			  (setq-local completion-at-point-functions
;; 						  '(tags-completion-at-point-function))))

(add-hook 'eshell-mode-hook 'company-mode)

(use-package fish-completion
	:straight t
	:hook (eshell-mode . fish-completion-mode))

(general-define-key :states 'insert
					:keymaps 'eshell-mode-map
					"<tab>" 'company-select-next)
