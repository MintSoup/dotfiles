;;; -*- lexical-binding: t -*-
(use-package all-the-icons-dired
	:straight t
	:config
	:hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-rsync
	:straight t)

(use-package diredfl
	:straight t
	:config
	:hook (dired-mode . diredfl-mode))

(use-package dired-hide-dotfiles
	:straight t
	:init
	(setq dired-hide-dotfiles-verbose nil)
	:config
	:hook (dired-mode . dired-hide-dotfiles-mode))

(defun up-alternate-file ()
	(interactive)
	(find-alternate-file ".."))

(general-define-key :keymaps 'dired-mode-map :states '(normal visual motion) "SPC" nil)
(my-local-leader :keymaps 'dired-mode-map
	"m" '(dired-hide-dotfiles-mode :wk "Toggle dotfiles")
	"n" '(dired-number-of-marked-files :wk "Show number of marked files")
	"o" '(dired-omit-mode :wk "Toggle Omit Mode"))

(general-define-key :keymaps 'dired-mode-map :states 'normal
					"l" '(dired-find-alternate-file :wk "Visit directory")
					"h" '(up-alternate-file :wk "Up directory"))

(setq dired-listing-switches "-alh")

(add-hook 'dired-mode-hook 'dired-omit-mode)
(put 'dired-find-alternate-file 'disabled nil)
