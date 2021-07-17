(setq my-skippable-buffer-regexp "^\*.+\*$")

(defun my-change-buffer (change-buffer)
	"Call CHANGE-BUFFER until `my-skippable-buffer-regexp' doesn't match."
	(let ((initial (buffer-name)))
		(funcall change-buffer)
		(cl-loop while
				 (string-match-p my-skippable-buffer-regexp (buffer-name)) do
				 (if (eq (buffer-name) initial)
						 (cl-return)
					 (funcall change-buffer)))))

(defun +!tmp-next-buffer ()

	"Temporary function to deal with current next-buffer bug"
	(previous-buffer)
	(next-buffer)
	(next-buffer))

(defun my-next-buffer ()
	"Variant of `next-buffer' that skips `my-skippable-buffer-regexp'."
	(interactive)
	(my-change-buffer '+!tmp-next-buffer))

(defun my-previous-buffer ()
	"Variant of `previous-buffer' that skips `my-skippable-buffer-regexp'."
	(interactive)
	(my-change-buffer 'previous-buffer))

(defun remove-scratch-buffer ()
	(if (get-buffer "*scratch*")
			(kill-buffer "*scratch*")))

(defun +kill-current-buffer ()
	(interactive)
	(unless (string-equal (buffer-name) "*dashboard*")
		(kill-current-buffer)))

(use-package all-the-icons-ibuffer
	:straight t
	:config
	(all-the-icons-ibuffer-mode))

(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
