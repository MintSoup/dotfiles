;;; -*- lexical-binding: t -*-
(add-hook 'eshell-mode-hook
		  (lambda ()
			  (setq-local company-backends '((company-files company-capf)))
			  (setq tab-width 4)))

(add-hook 'eshell-mode-hook 'company-mode)

(general-define-key :states 'insert
					:keymaps 'eshell-mode-map
					"<tab>" 'company-select-next)