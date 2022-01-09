;;; -*- lexical-binding: t -*-
;; EVIL
(use-package evil
	:straight t
	:init
	(setq evil-want-integration t ;; This is optional since it's already set to t by default.
		  evil-want-keybinding nil
		  evil-vsplit-window-right t
		  evil-split-window-below t
		  evil-want-Y-yank-to-eol t
		  evil-undo-system 'undo-redo
		  evil-search-module 'evil-search
		  evil-ex-search-vim-style-regexp t
		  evil-want-C-u-scroll t
		  evil-respect-visual-line-mode t
		  evil-lookup-func 'man)
	:config
	;; use C to dd without yanking
	(evil-define-operator evil-delete-line-without-yank (beg end type reg yank-handler)
		"Delete line without yanking."
		:motion evil-line-or-visual-line
		(evil-delete-whole-line beg end type ?_ yank-handler))
	;; Same for x
	(evil-define-operator evil-delete-char-without-yank (beg end type reg yank-handler)
		"Delete char without yanking."
		:motion evil-forward-char
		(evil-delete-char beg end type ?_))

	(evil-define-operator evil-org-delete-char-without-yank (count beg end type register)
		"`org-delete-char' without yanking"
		:motion evil-forward-char
		(interactive "p<R><x>")
		(evil-org-delete-char count beg end type ?_))

	;; Text objects from doom
	(evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
		"Text object to select the whole buffer."
		(evil-range (point-min) (point-max) type))

	(evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
		"Text object to select the top-level Lisp form or function definition at point."
		(cl-destructuring-bind (beg . end)
				(bounds-of-thing-at-point 'defun)
			(evil-range beg end type)))

	(evil-define-text-object +evil:inner-url-txtobj (count &optional _beg _end type)
		"Text object to select the inner url at point. This excludes the protocol and querystring."
		(cl-destructuring-bind (beg . end)
				(bounds-of-thing-at-point 'url)
			(evil-range
			 (save-excursion
				 (goto-char beg)
				 (re-search-forward "://" end t))
			 (save-excursion
				 (goto-char end)
				 (- (if-let (pos (re-search-backward "[?#]" beg t))
							pos
						end)
					(if (evil-visual-state-p)
							1
						0))) type)))

	(evil-define-text-object +evil:outer-url-txtobj (count &optional _beg _end type)
		"Text object to select the whole url at point."
		(cl-destructuring-bind (beg . end)
				(bounds-of-thing-at-point 'url)
			(evil-range
			 beg (- end (if (evil-visual-state-p) 1 0))
			 type)))

	(general-define-key :keymaps 'outer
						"u" '+evil:outer-url-txtobj
						"g" '+evil:whole-buffer-txtobj
						"f" '+evil:defun-txtobj
						"a" 'evil-outer-arg)

	(general-define-key :keymaps 'inner
						"u" '+evil:inner-url-txtobj
						"g" '+evil:whole-buffer-txtobj
						"f" '+evil:defun-txtobj
						"a" 'evil-inner-arg)

	(general-define-key :states 'normal
						"C" 'evil-delete-line-without-yank
						"x" 'evil-delete-char-without-yank
						"C-a" 'evil-numbers/inc-at-pt
						"M-j" 'evil-collection-unimpaired-move-text-down
						"M-k" 'evil-collection-unimpaired-move-text-up
						"C-S-a" 'evil-numbers/dec-at-pt
						"go" 'indent-buffer)

	(defun +evil/shift-right ()
		"vnoremap < <gv"
		(interactive)
		(call-interactively #'evil-shift-right)
		(evil-normal-state)
		(evil-visual-restore))

	(defun +evil/shift-left ()
		"vnoremap > >gv"
		(interactive)
		(call-interactively #'evil-shift-left)
		(evil-normal-state)
		(evil-visual-restore))

	(evil-define-motion evil-lookup ()
		"Look up the keyword at point. Calls `evil-lookup-func' interactively."
		(call-interactively evil-lookup-func))

	(general-define-key :states 'visual
						">" '+evil/shift-right
						"<" '+evil/shift-left)

	(evil-mode))

;; (use-package undo-fu
;; 	:straight t)

(use-package evil-collection
	:straight t
	:after evil
	:config
	(evil-collection-init))

(use-package evil-lion
	:straight t
	:after evil
	:config
	(evil-lion-mode))

(use-package evil-snipe
	:straight t
	:after evil
	:init
	(setq evil-snipe-smart-case t
		  evil-snipe-scope 'whole-buffer
		  evil-snipe-repeat-scope 'whole-buffer
		  evil-snipe-char-fold t)
	:config
	(general-define-key :states 'insert
						:keymaps 'evil-snipe-local-mode-map
						"S" nil)
	(evil-snipe-mode +1)
	(evil-snipe-override-mode +1))

(use-package evil-easymotion
	:straight t
	:after evil
	:config
	(evilem-default-keybindings "gs")
	(general-define-key :keymaps 'evilem-map
						"s" 'evil-avy-goto-char-2
						"S" 'evil-avy-goto-char-2
						"a" (evilem-create (list 'evil-forward-arg 'evil-backward-arg)))

	(setq avy-background t))

(use-package evil-surround
	:straight t
	:config
	(global-evil-surround-mode 1))

(use-package evil-numbers
	:after evil
	:straight t)

(use-package evil-args
	:straight t
	:after evil)

(use-package evil-commentary
	:straight t
	:after evil
	:config
	(general-define-key :states '(normal visual)
						"gc" 'evil-commentary))

(use-package evil-exchange
	:straight t
	:after evil
	:config
	(general-define-key :states '(normal visual)
						"gx" 'evil-exchange
						"gX" 'evil-exchange-cancel))

(use-package evil-goggles
	:straight t
	:after evil
	:init
	(setq evil-goggles-duration 0.2
		  evil-goggles-enable-shift nil
		  evil-goggles-enable-delete nil
		  evil-goggles-enable-paste nil
		  evil-goggles-enable-change nil
		  pulse-flag t)
	:config
	(evil-goggles-mode +1))

(use-package anzu
	:straight t)

(use-package evil-anzu
	:straight t
	:config (global-anzu-mode +1))
