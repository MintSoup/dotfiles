(with-eval-after-load 'erc
	(my-local-leader :keymaps 'erc-mode-map
		"q" '(erc-cmd-QUERY :wk "Open DM")
		"j" '(erc-join-channel :wk "Join channel"))

	(general-define-key :keymaps 'erc-mode-map :states 'normal
						"<return>" 'erc-send-current-line)
	(general-define-key :keymaps 'erc-mode-map :states 'insert
						"C-SPC" 'company-complete)

	(setq erc-auto-query 'window-noselect)

	(setq erc-default-server "irc.libera.chat"
		  erc-nick "mintsoup")

    (add-hook 'erc-mode-hook 'visual-fill-column-mode)
    (add-hook 'erc-mode-hook 'erc-pcomplete-disable)
    (add-hook 'erc-mode-hook 'company-mode)
    (add-hook 'erc-mode-hook 'variable-pitch-mode)
	(add-hook 'erc-mode-hook
			  (lambda ()
				  (setq-local company-idle-delay nil)))

	(set-face-attribute 'erc-nick-default-face nil :inherit 'fixed-pitch)
	(set-face-attribute 'erc-my-nick-face nil :inherit 'fixed-pitch))
