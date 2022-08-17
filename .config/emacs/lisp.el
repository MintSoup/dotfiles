;;; -*- lexical-binding: t -*-
(use-package evil-cleverparens
	:straight t
	:hook (emacs-lisp-mode . evil-cleverparens-mode)
	:hook (scheme-mode . evil-cleverparens-mode)
	:hook (lisp-mode . evil-cleverparens-mode)
	:hook (evil-cleverparens-mode . turn-off-visual-line-mode)
	:config
	(setq evil-cleverparens-indent-afterwards nil) ;; cp does it too early
	(general-define-key :keymaps 'outer "f" '+evil:defun-txtobj)
	(general-define-key :keymaps 'inner "f" '+evil:defun-txtobj)
	(general-define-key :keymaps 'evil-cleverparens-mode-map
						:states '(normal insert)
						"C-(" '+wrap-and-edit-next))

(use-package sly
	:straight t
	:hook (sly-mrepl-mode . company-mode)
	:init
	(setq inferior-lisp-program "sbcl"))

(general-define-key :keymaps 'sly-mrepl-mode-map
					:states 'insert
					"<up>" 'comint-previous-input
					"<down>" 'comint-next-input
					"," 'self-insert-command
					"C-," 'sly-mrepl-shortcut)

(my-local-leader :keymaps 'lisp-mode-map
	"i" '(sly-documentation :wk "Documentation")
	"I" '(sly-documentation-lookup :wk "Online Documentation")

	"p" '(sly-pprint-eval-last-expression :wk "Eval and pretty print")
	"l" '(sly-load-file :wk "Load file")

	"d" '(sly-disassemble-definition :wk "Disassemble definition")
	"D" '(sly-disassemble-symbol :wk "Disassemble symbol")

	"e" '(:ignore t :wk "Edit")
	"ed" '(sly-edit-definition :wk "Definition")
	"ev" '(sly-edit-value :wk "Value")

	"m" '(:ignore t :wk "Macro")
	"me" '(sly-macroexpand-1 :wk "Expand")
	"mE" '(sly-macroexpand-1-inplace :wk "Expand inplace")
	"mr" '(sly-macroexpand-all :wk "Recursively")
	"mR" '(sly-macroexpand-all-inplace :wk "Recursively inplace")

	"s" '(sly-mrepl-sync :wk "Sync package"))


;; (defalias 'sp-forward-barf-sexp 'paredit-forward-barf-sexp)

(general-define-key :keymaps 'evil-cleverparens-mode-map
					:states '(normal visual)
					"s" 'evil-snipe-s
					"x" 'evil-cp-delete-char-without-yank
					"C" 'evil-cp-delete-line-without-yank
					"M-y" 'evil-paste-pop)


(defun evil-cp--balanced-block-p-noyank (beg end)
	(let* ((region (evil-yank-rectangle beg end ?_)))
		(with-temp-buffer
			(insert region)
			(sp-region-ok-p (point-min) (point-max)))))

(defun turn-off-visual-line-mode ()
	(interactive)
	(visual-line-mode -1))

(defun indent-this-sexp (BEG END &optional TYPE REGISTER YANK-HANDLER)
	(when (evil-cp--inside-any-form-p)
		(save-excursion
			(evil-cp--backward-up-list)
			(indent-sexp))))

(defun +wrap-and-edit-next (count)
	(interactive "p")
	(evil-cp-wrap-next-round count)
	(evil-cp-insert 1))

(advice-add 'evil-cp--balanced-block-p :override 'evil-cp--balanced-block-p-noyank)
(advice-add 'evil-cp-delete :after 'indent-this-sexp)

(evil-define-operator evil-cp-delete-line-without-yank (beg end type reg yank-handler)
	"Delete line without yanking while keeping parenthesis balanced."
	:motion evil-line-or-visual-line
	(evil-cp-delete beg end type ?_ yank-handler))

(evil-define-operator evil-cp-delete-char-without-yank (beg end type reg yank-handler)
	"Delete char without yanking."
	:motion evil-forward-char
	(interactive "<R><x>")
	(evil-cp-delete-char-or-splice beg end type ?_ yank-handler))

(add-hook 'lisp-mode-hook 'highlight-quoted-mode)

(require 'smartparens-config)
