(use-package company
	:straight t
	:config
	(setq company-idle-delay 0
		  company-minimum-prefix-length 1)
	(general-define-key :keymaps '(company-active-map company-search-map company-tng-map)
						"C-w" nil
						"C-h" nil
						"<f1>" nil)
	(global-company-mode))
