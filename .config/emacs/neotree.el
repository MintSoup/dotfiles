;;; -*- lexical-binding: t -*-
(use-package neotree
	:straight t
	:config
	(setq doom-themes-neotree-enable-variable-pitch t
		  doom-themes-neotree-file-icons t
		  doom-themes-neotree-folder-size 1.15
		  doom-themes-neotree-chevron-size 1.0)
	(doom-themes-neotree-config)
	(add-hook 'neo-after-create-hook
			  #'(lambda (_)
					(with-current-buffer (get-buffer neo-buffer-name)
						(setq truncate-lines t
							  word-wrap nil
							  mode-line-format nil)
						(make-local-variable 'auto-hscroll-mode)
						(setq auto-hscroll-mode nil))))


	(my-open-leader "p" 'neotree-toggle))
