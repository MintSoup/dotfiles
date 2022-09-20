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

(use-package dired-du
  :straight t
  :init
  (setq dired-du-size-format t))

(add-hook 'dired-mode-hook 'auto-revert-mode)

(defun up-alternate-file ()
  (interactive)
  (find-alternate-file ".."))

(general-define-key :keymaps 'dired-mode-map :states '(normal visual motion) "SPC" nil)

(defun rcd/dired-view ()
  "View files, either as HTML or media"
  (interactive)
  (let* ((files (dired-get-marked-files))
		 (how-many (length files))
		 (extensions (mapcar 'file-name-extension files))
		 (extensions (mapcar 'downcase extensions)))
	(if (> how-many 1) (xdg-open-files files)
	  (xdg-open (car files) t))))

(defun xdg-open (file &optional async)
  "Opens file with xdg-open. Without optional argument ASYNC, it will wait for the file to finish playing or review."
  (if async
	  (start-process "dired-xdg" " dired-xdg-open" "xdg-open" file)
	(shell-command command)))

(defun xdg-open-files (files)
  "Opens list of files with xdg-open one by one, waiting for each to finish."
  (dolist (file files)
	(xdg-open file)))

(my-local-leader :keymaps 'dired-mode-map
  "m" '(dired-hide-dotfiles-mode :wk "Toggle dotfiles")
  "n" '(dired-number-of-marked-files :wk "Show number of marked files")
  "f" '(rcd/dired-view :wk "XDG open")
  "o" '(dired-omit-mode :wk "Toggle Omit Mode")
  "d" '(dired-du-mode :wk "Dired-du"))

(general-define-key :keymaps 'dired-mode-map :states 'normal
					"l" '(dired-find-alternate-file :wk "Visit directory")
					"h" '(up-alternate-file :wk "Up directory"))

(setq dired-listing-switches "-alh"
	  dired-dwim-target t
	  all-the-icons-dired-monochrome nil)

(add-hook 'dired-mode-hook 'dired-omit-mode)
(put 'dired-find-alternate-file 'disabled nil)
