;;; -*- lexical-binding: t -*-
(use-package shackle
	:straight t
	:functions org-switch-to-buffer-other-window
	:commands shackle-display-buffer
	:hook (after-init . shackle-mode)
	:config
	(with-no-warnings
		(defvar shackle--popup-window-list nil) ; all popup windows
		(defvar-local shackle--current-popup-window nil) ; current popup window
		(put 'shackle--current-popup-window 'permanent-local t)

		(defun shackle-display-buffer-hack (fn buffer alist plist)
			(let ((window (funcall fn buffer alist plist)))
				(setq shackle--current-popup-window window)

				(when (plist-get plist :autoclose)
					(push (cons window buffer) shackle--popup-window-list))
				(with-current-buffer buffer
					(add-hook 'kill-buffer-hook
							  (lambda ()
								  (unless (one-window-p)
									  (delete-window)))
							  0 t)) ;; Kill window when killing popup buffers
				window))

        (defun +shackle/quit-window (&optional kill window)
            (interactive "P")
            (kill-buffer-and-window))
		(general-define-key :keymaps '(Info-mode-map helpful-mode-map) :states 'normal "q" '+shackle/quit-window)

		;; HACK: compatibility issue with `org-switch-to-buffer-other-window'
		(advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)
		(advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

		;; rules
		(setq shackle-default-size 0.4
			  shackle-default-alignment 'below
			  shackle-default-rule nil
			  shackle-rules
			  '((("*Help*" "*Apropos*") :select t :size 0.3 :align below :autoclose t)
				(compilation-mode :select t :size 0.3 :align below :autoclose t)
				(comint-mode :select t :size 0.4 :align below :autoclose t)
				("*Completions*" :size 0.3 :align below :autoclose t)
				("*Pp Eval Output*" :size 15 :align below :autoclose t)
				("*Backtrace*" :select t :size 15 :align below)
				(("*Warnings*" "*Messages*") :size 0.3 :align below :autoclose t)
				("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align below :autoclose t)
				("\\*[Wo]*Man.*\\*" :regexp t :select t :align below :autoclose t)
				("*Calendar*" :select t :size 0.3 :align below)
				(("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 :align below :volatile)
				("^\\*vc-.*\\*$" :regexp t :size 0.3 :align below :autoclose t)
				("*gud-debug*" :select t :size 0.4 :align below :autoclose t)
				;; ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align below)
				(" *undo-tree*" :select t)
				("*quickrun*" :size 0.3 :align below)
				("*tldr*" :size 0.4 :align below :autoclose t)
				("*osx-dictionary*" :size 20 :align below :autoclose t)
				("*Youdao Dictionary*" :size 15 :align below :autoclose t)
				("*Finder*" :select t :size 0.3 :align below :autoclose t)
				("^\\*macro expansion\\**" :regexp t :size 0.4 :align below)
				("^\\*elfeed-entry" :regexp t :size 0.7 :align below :autoclose t)
				(transmission-mode :size 1 :align below :autoclose t :same t)
				(" *Install vterm* " :size 0.35 :same t :align below)
				("*vterm*" :size 0.35 :align below)
				(("*Paradox Report*" "*package update results*") :size 0.2 :align below :autoclose t)
				("*Package-Lint*" :size 0.4 :align below :autoclose t)
				("*How Do You*" :select t :size 0.5 :align below :autoclose t)
				("* Guile REPL *" :size 0.3 :align below :autoclose t)
				("*Geiser dbg*" :size 0.3 :align below :autoclose t)

				(("*Org Agenda*" " *Agenda Commands*" " *Org todo*" "*Org Dashboard*" "*Org Select*") :select t :size 0.1 :align below :autoclose t)
				(("\\*Capture\\*" "^CAPTURE-.*\\.org*") :regexp t :select t :size 0.3 :align below :autoclose t)

				("*ert*" :size 15 :align below :autoclose t)
				(overseer-buffer-mode :size 15 :align below :autoclose t)

				(" *Flycheck checkers*" :select t :size 0.3 :align below :autoclose t)
				((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
				 :select t :size 0.25 :align below :autoclose t)

				(("*lsp-help*" "*lsp session*") :size 0.3 :align below :autoclose t)
				("*DAP Templates*" :select t :size 0.4 :align below :autoclose t)
				(dap-server-log-mode :size 15 :align below :autoclose t)
				("*rustfmt*" :select t :size 0.3 :align below :autoclose t)
				((rustic-compilation-mode rustic-cargo-clippy-mode rustic-cargo-outdated-mode rustic-cargo-test-mode)
				 :select t :size 0.3 :align below :autoclose t)

				(profiler-report-mode :select t :size 0.5 :align below)
				("*ELP Profiling Restuls*" :select t :size 0.5 :align below)

				((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 :align below)
				("*prolog*" :size 0.4 :align below)

				(("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align below :autoclose t)
				(godoc-mode :select t :size 0.4 :align below :autoclose t)

				((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :size 0.4 :align below)
				(Buffer-menu-mode :select t :size 20 :align below :autoclose t)
				(gnus-article-mode :select t :size 0.7 :align below :autoclose t)
				(magit-status-mode :select t :size 0.5 :align right :autoclose t)
				(helpful-mode :select t :size 0.4 :align below :autoclose t :volatile)
				((process-menu-mode cargo-process-mode) :select t :size 0.3 :align below :autoclose t)
				(list-environment-mode :select t :size 0.3 :align below :autoclose t)
				(tabulated-list-mode :size 0.4 :align below)))))
