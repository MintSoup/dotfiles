(use-package company
	:straight t
	:hook (prog-mode . company-mode)
	      (eshell-mode . company-mode)
	:config
	(setq company-idle-delay 0
		  company-minimum-prefix-length 1)
	(general-define-key :keymaps '(company-active-map company-search-map company-tng-map)
						"C-w" nil
						"C-h" nil
						"<f1>" nil)
	(setq company-backends
		  '(company-capf company-files
						 (company-dabbrev-code company-keywords)
						 company-dabbrev)))
