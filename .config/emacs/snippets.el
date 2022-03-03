;;; -*- lexical-binding: t -*-

(use-package yasnippet
	:straight t
	:hook (prog-mode . yas-minor-mode)
	:config
	(general-define-key
	 :keymaps 'yas-minor-mode-map
	 :states 'insert
	 "C-e" 'yas-expand
	 "\t" nil))

(use-package yasnippet-snippets
	:straight t
	:config
	(yas-reload-all))
