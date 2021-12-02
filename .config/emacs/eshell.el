;;; -*- lexical-binding: t -*-
(add-hook 'eshell-mode-hook
		  (lambda ()
			  (setq-local company-backends '((company-capf)))
			  (setq tab-width 4)
			  (company-mode +1)
			  (eshell-cmpl-mode -1)))
