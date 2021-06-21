(setq my-skippable-buffer-regexp "^\*.+\*$")

(defun my-change-buffer (change-buffer)
	"Call CHANGE-BUFFER until `my-skippable-buffer-regexp' doesn't match."
	(let ((initial (current-buffer)))
		(funcall change-buffer)
		(let ((first-change (current-buffer)))
			(catch 'loop
				(while (string-match-p my-skippable-buffer-regexp (buffer-name))
					(funcall change-buffer)
					(when (eq (current-buffer) first-change)
						(switch-to-buffer initial)
						(throw 'loop t)))))))

(defun my-next-buffer ()
	"Variant of `next-buffer' that skips `my-skippable-buffer-regexp'."
	(interactive)
	(my-change-buffer 'next-buffer))

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

(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
