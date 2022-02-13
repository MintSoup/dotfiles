;;; -*- lexical-binding: t -*-
(use-package company
	:straight t
	:hook (prog-mode . company-mode)
	:config
	(company-tng-mode)
	(setq company-idle-delay 0
		  company-minimum-prefix-length 1)
	(general-define-key :keymaps 'company-tng-map
						"C-w" nil
						"C-h" nil
						"<f1>" nil
						;; "C-c" 'company-complete
						"<tab>" 'company-select-next)
	(setq company-backends
		  '((company-capf company-files company-yasnippet
						  geiser-company-backend))))

(defun eshell-disable-company-on-tramp ()
	(when (and (fboundp 'company-mode)
               (file-remote-p default-directory))
		(company-mode -1)))

(add-hook 'eshell-mode-hook 'eshell-disable-company-on-tramp)
