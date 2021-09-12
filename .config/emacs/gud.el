;;; -*- lexical-binding: t -*-

(general-define-key :states 'normal
					"<f12>"		'gud-finish
					"<f11>"		'gud-step
					"<f10>"		'gud-next
					"<f9>"		'gud-break
					"<f5>"		'gud-cont
					"<delete>"	'gud-remove)
(setq gdb-many-windows t)

(defun my-gdb-setup-windows ()
	"Lay out the window pattern for option `gdb-many-windows'."
	(if gdb-default-window-configuration-file
			(gdb-load-window-configuration
			 (if (file-name-absolute-p gdb-default-window-configuration-file)
					 gdb-default-window-configuration-file
				 (expand-file-name gdb-default-window-configuration-file
								   gdb-window-configuration-directory)))
		;; Create default layout as before.
		(gdb-get-buffer-create 'gdb-locals-buffer)
		(gdb-get-buffer-create 'gdb-stack-buffer)

		(delete-other-windows)
		(let* ((win0 (selected-window))
			   (win1 (split-window nil (floor (* (window-width) 0.66)) t)))

			(select-window win1)
			(evil-normal-state)
			(set-window-dedicated-p win1 t)

			(let* ((win2 (split-window nil (floor (* (window-height) 0.75))))
				   (win3 (split-window nil (floor (* (window-height) 0.66))))
				   (win4 (split-window nil (floor (* (window-height) 0.5)))))

				(set-window-buffer win0 (or (gdb-get-source-buffer)
											(list-buffers-noselect)))

				(with-current-buffer (gdb-stack-buffer-name)
					(evil-exit-emacs-state))

				(with-current-buffer (gdb-inferior-io-name)
					(evil-normal-state))

				(with-current-buffer (gdb-locals-buffer-name)
					(evil-normal-state))

				(gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil win3)
				(gdb-set-window-buffer (gdb-get-buffer-create 'gdb-locals-buffer) nil win4)
				(gdb-set-window-buffer (gdb-stack-buffer-name) nil win2)
				(setq gdb-source-window-list `(,win0))
				(select-window win0)))))

(advice-add 'gdb-setup-windows :override #'my-gdb-setup-windows)
(general-define-key :keymaps 'gdb-frames-mode-map
					:states 'normal
					"RET" 'gdb-select-frame)
