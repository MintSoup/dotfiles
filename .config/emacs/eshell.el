;;; -*- lexical-binding: t -*-
(add-hook 'eshell-mode-hook
		  (lambda ()
			  (setq-local company-backends '((company-capf))
						  completion-at-point-functions '(fish-completion-shell-complete t))
			  (setq tab-width 4)
			  (company-mode +1)))


(use-package fish-completion
	:straight t)
