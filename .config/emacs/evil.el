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
		  evil-undo-system 'undo-fu
		  evil-search-module 'evil-search
		  evil-ex-search-vim-style-regexp t
		  evil-want-C-u-scroll t
		  evil-respect-visual-line-mode t)
	:config
	;; use C to dd without yanking
	(evil-define-operator evil-delete-line-without-yank (beg end type reg yank-handler)
		"Delete line without yanking."
		:motion evil-line-or-visual-line
		(evil-delete-whole-line beg end type ?_ yank-handler))
	;; Same for x
	(evil-define-operator evil-org-delete-char-without-yank (count beg end type register)
		"Same as evil-line-or-visual-line but without yank."
		:motion evil-forward-char
		(interactive "p<R><x>")
		(evil-org-delete-char count beg end type ?_))

	;; Text objects from doom
	(evil-define-text-object +evil:whole-buffer-txtobj (count &optional _beg _end type)
		"Text object to select the whole buffer."
		(evil-range (point-min) (point-max) type))

	(evil-define-text-object +evil:defun-txtobj (count &optional _beg _end type)
		"Text object to select the top-level Lisp form or function definition at
point."
		(cl-destructuring-bind (beg . end)
				(bounds-of-thing-at-point 'defun)
			(evil-range beg end type)))

	(evil-define-text-object +evil:inner-url-txtobj (count &optional _beg _end type)
		"Text object to select the inner url at point.
This excludes the protocol and querystring."
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
						0)))
			 type)))

	(evil-define-text-object +evil:outer-url-txtobj (count &optional _beg _end type)
		"Text object to select the whole url at point."
		(cl-destructuring-bind (beg . end)
				(bounds-of-thing-at-point 'url)
			(evil-range
			 beg (- end (if (evil-visual-state-p) 1 0))
			 type)))

	(evil-define-text-object +evil:inner-any-quote (count &optional beg end type)
		"Select the closest inner quote."
		(let ((evil-textobj-anyblock-blocks
			   '(("'" . "'")
				 ("\"" . "\"")
				 ("`" . "`")
				 ("‘" . "’")
				 ("“" . "”"))))
			(evil-textobj-anyblock-inner-block count beg end type)))

	(evil-define-text-object +evil:outer-any-quote (count &optional beg end type)
		"Select the closest outer quote."
		(let ((evil-textobj-anyblock-blocks
			   '(("'" . "'")
				 ("\"" . "\"")
				 ("`" . "`")
				 ("‘" . "’")
				 ("“" . "”"))))
			(evil-textobj-anyblock-a-block count beg end type)))

	(general-define-key :keymaps 'outer
						"u" '+evil:outer-url-txtobj
						"q" '+evil:outer-any-quote
						"g" '+evil:whole-buffer-txtobj
						"f" '+evil:defun-txtobj
						"a" 'evil-outer-arg)

	(general-define-key :keymaps 'inner
						"u" '+evil:inner-url-txtobj
						"q" '+evil:inner-any-quote
						"g" '+evil:whole-buffer-txtobj
						"f" '+evil:defun-txtobj
						"a" 'evil-inner-arg)

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

	(general-define-key :states 'visual
						">" '+evil/shift-right
						"<" '+evil/shift-left)

	(evil-mode))

(use-package evil-snipe
	:straight t
	:after evil
	:init
	(setq evil-snipe-smart-case t
		  evil-snipe-scope 'whole-buffer
		  evil-snipe-repeat-scope 'whole-buffer
		  evil-snipe-char-fold t)
	:config
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
	:after evil
	:config (global-evil-surround-mode 1))

(use-package evil-numbers
	:after evil
	:straight t)

(use-package evil-collection
	:straight t
	:after evil
	:config
	(evil-collection-init))

(use-package evil-args
	:straight t
	:after evil)

(use-package evil-textobj-anyblock
	:straight t
	:after evil
	:config
	(general-define-key :states '(normal visual)
						"gx" 'evil-exchange


						"gX" 'evil-exchange-cancel))

(use-package evil-commentary
	:straight t
	:after evil
	:config
	(general-define-key :states '(normal visual)
						"gc" 'evil-commentary))

(use-package evil-exchange
	:straight t
	:after evil)

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

(use-package undo-fu
	:straight t)