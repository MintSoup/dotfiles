;;; -*- lexical-binding: t -*-
(use-package evil-cleverparens
	:straight t
	:hook (emacs-lisp-mode . evil-cleverparens-mode)
	:hook (scheme-mode . evil-cleverparens-mode)
	:hook (evil-cleverparens-mode . turn-off-visual-line-mode)
	:config
	(general-define-key :keymaps 'outer "f" '+evil:defun-txtobj)
	(general-define-key :keymaps 'inner "f" '+evil:defun-txtobj))


(general-define-key :keymaps 'evil-cleverparens-mode-map
					:states '(normal visual)
					"s" 'evil-snipe-s
					"x" 'evil-cp-delete-char-without-yank
					"C" 'evil-cp-delete-line-without-yank)


(defun evil-cp--balanced-block-p-noyank (beg end)
	(let* ((region (evil-yank-rectangle beg end ?_)))
		(with-temp-buffer
			(insert region)
			(sp-region-ok-p (point-min) (point-max)))))

(defun turn-off-visual-line-mode ()
	(interactive)
	(visual-line-mode -1))

(advice-add 'evil-cp--balanced-block-p :override 'evil-cp--balanced-block-p-noyank)

(evil-define-operator evil-cp-delete-line-without-yank (beg end type reg yank-handler)
	"Delete line without yanking while keeping parenthesis balanced."
	:motion evil-line-or-visual-line
	(evil-cp-delete beg end type ?_ yank-handler))

(evil-define-operator evil-cp-delete-char-without-yank (beg end type reg yank-handler)
	"Delete char without yanking."
	:motion evil-forward-char
	(interactive "<R><x>")
	(evil-cp-delete-char-or-splice beg end type ?_ yank-handler))
