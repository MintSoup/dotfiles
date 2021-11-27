;;; -*- lexical-binding: t -*-
(use-package company
	:straight t
	:hook (prog-mode . company-mode)
	:config
	(company-tng-mode)
	(setq company-idle-delay 0
		  company-minimum-prefix-length 1)
	(general-define-key :keymaps '(company-active-map company-search-map company-tng-map)
						"C-w" nil
						"C-h" nil
						"<f1>" nil
						"<tab>" 'company-select-next)
	(setq company-backends
		  '((company-capf company-files geiser-company-backend company-keywords))))
