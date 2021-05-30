(with-eval-after-load 'erc
	(my-local-leader :keymaps 'erc-mode-map
		"q" '(erc-cmd-QUERY :wk "Open DM")
		"j" '(erc-join-channel :wk "Join channel"))

	(general-define-key :keymaps 'erc-mode-map :states 'normal
						"<return>" 'erc-send-current-line)

	(setq erc-auto-query 'window-noselect)

	(setq erc-default-server "irc.libera.chat"
		  erc-nick "mintsoup")

	(add-hook 'erc-mode-hook 'visual-fill-column-mode)
	(add-hook 'erc-mode-hook 'erc-pcomplete-disable)
	(add-hook 'erc-mode-hook 'company-mode))
