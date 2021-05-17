(my-local-leader :keymaps 'erc-mode-map
	"q" '(erc-cmd-QUERY :wk "Open DM")
	"j" '(erc-join-channel :wk "Join channel"))
(setq erc-auto-query 'bury)
(general-define-key :keymaps 'erc-mode-map :states 'normal
					"<return>" 'erc-send-current-line)
