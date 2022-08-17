;;; -*- lexical-binding: t -*-

(defun major-mode-matcher (mode)
	(lambda (buf act)
		(with-current-buffer buf
			(eq major-mode mode))))

(defun side-window-clause (matcher &rest params)
	(declare (indent defun))
	`(,(if (symbolp matcher)
			   (major-mode-matcher matcher)
		   matcher)
	  (display-buffer-reuse-window
	   +display-buffer-in-side-window)
	  ,@(cl--plist-to-alist params)))

(defun +display-buffer-in-side-window (buf alist)
	(select-window (display-buffer-in-side-window buf alist)))


(defun display-buffer-in-direction-kill (buf alist)
	(with-current-buffer buf
		(add-hook 'kill-buffer-hook
				  (lambda ()
					  (unless (one-window-p)
						  (-when-let (window (get-buffer-window))
							  (delete-window window))))
				  0 t))
	(display-buffer-in-direction buf alist))

(setq display-buffer-alist
      `(,(side-window-clause 'helpful-mode 'window-height 0.4)
		,(side-window-clause 'flycheck-error-list-mode 'window-height 0.4)
		,(side-window-clause (rx bos "*ielm*" eos))

		(,(rx bos (or "*vterm" "*eshell") (* anychar) "*"
			  (? "<" (+ digit) ">") eos)
		 (display-buffer-reuse-window
		  display-buffer-in-side-window)
		 (window-height . 0.35))

		,(side-window-clause
			 (rx bos "*Async Shell Command*" eos)
			 'window-height 0.35)
		,(side-window-clause
			 (rx bos (or "*Geiser" "*st-util" "*quickrun" "*sly") (* anychar) "*"
				 (? "<" (+ digit) ">") eos)
			 'window-height 0.35)

		(,(rx bos "*" (or (group (? "Wo") "Man" (* any))
						  "info") "*" eos)
		 (display-buffer-reuse-window
		  display-buffer-in-direction-kill)
		 (direction . rightmost)
		 (window-width . 0.5))

		(,(major-mode-matcher 'magit-status-mode)
		 (display-buffer-reuse-window
		  display-buffer-in-direction-kill)
		 (direction . right)
		 (window-width . 0.5))

		(,(major-mode-matcher 'transmission-mode)
		 (display-buffer-reuse-window
		  display-buffer-same-window)
		 (inhibit-same-window . nil))

		(,(major-mode-matcher 'compilation-mode)
		 (display-buffer-reuse-window
		  +display-buffer-in-side-window)
		 (window-height . 0.35))

		,(side-window-clause
			 (rx bos (or "*Warnings*" "*Messages*") eos)
			 'window-height 0.3)))


;; (use-package shackle
;; 	:straight t
;; 	:functions org-switch-to-buffer-other-window
;; 	:commands shackle-display-buffer
;; 	:hook (after-init . shackle-mode)
;; 	:config
;; 	(with-no-warnings
;; 		(defvar shackle--popup-window-list nil) ; all popup windows
;; 		(defvar-local shackle--current-popup-window nil) ; current popup window
;; 		(put 'shackle--current-popup-window 'permanent-local t)

;; 		(defun shackle-display-buffer-hack (fn buffer alist plist)
;; 			(let ((window (funcall fn buffer alist plist)))
;; 				(setq shackle--current-popup-window window)

;; 				(when (plist-get plist :autoclose)
;; 					(push (cons window buffer) shackle--popup-window-list))
;;			(with-current-buffer buffer
;;				(add-hook 'kill-buffer-hook
;;						  (lambda ()
;;							  (unless (one-window-p)
;;								  (-when-let (window (get-buffer-window))
;;									  (delete-window window))))
;;						  0 t)) ;; Kill window when killing popup buffers
;; 				window))

;;         (defun +shackle/quit-window (&optional kill window)
;;             (interactive "P")
;;             (kill-buffer-and-window))
;; 		(general-define-key :keymaps '(Info-mode-map helpful-mode-map) :states 'normal "q" '+shackle/quit-window)

;; 		;; HACK: compatibility issue with `org-switch-to-buffer-other-window'
;; 		(advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)
;; 		(advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

;; 		;; rules
;; 		(setq shackle-default-size 0.4
;; 			  shackle-default-alignment 'below
;; 			  shackle-default-rule nil
;; 			  shackle-rules
;; 			  '((("*Help*" "*Apropos*") :select t :size 0.3 :align below :autoclose t)
;; 				(compilation-mode :select t :size 0.3 :align below :autoclose t)
;; 				(comint-mode :select t :size 0.4 :align below :autoclose t)
;; 				("*Completions*" :size 0.3 :align below :autoclose t)
;; 				("*Pp Eval Output*" :size 15 :align below :autoclose t)
;; 				("*Backtrace*" :select t :size 15 :align below)
;; 				(("*Warnings*" "*Messages*") :size 0.3 :align below :autoclose t)
;; 				("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align below :autoclose t)
;; 				("\\*[Wo]*Man.*\\*" :regexp t :select t :align right :size 0.55 :autoclose t)
;; 				("*Calendar*" :select t :size 0.3 :align below)
;; 				(("*shell*" "*ielm*") :popup t :size 0.3 :align below :volatile)
;; 				("\\*eshell.*" :other t :popup :t 'window-height 0.35 :regexp t)
;; 				("^\\*vc-.*\\*$" :regexp t :size 0.3 :align below :autoclose t)
;; 				;; ("*gud-debug*" :select t :size 0.4 :align below :autoclose t)
;; 				;; ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 :align below)
;; 				(" *undo-tree*" :select t)
;; 				("*quickrun*" :size 0.3 :align below)
;; 				("*tldr*" :size 0.4 :align below :autoclose t)
;; 				("*osx-dictionary*" :size 20 :align below :autoclose t)
;; 				("*Youdao Dictionary*" :size 15 :align below :autoclose t)
;; 				("*Finder*" :select t :size 0.3 :align below :autoclose t)
;; 				("^\\*macro expansion\\**" :regexp t :size 0.4 :align below)
;; 				("^\\*elfeed-entry" :regexp t :size 0.7 :align below :autoclose t)
;; 				(transmission-mode :size 1 :align below :autoclose t :same t)
;; 				(Info-mode :size 0.5 :select t :align right :autoclose t)
;; 				(" *Install vterm* " :size 0.35 :same t :align below)
;; 				("\\*vterm.*\\*" :regexp t :size 0.35 :align below)
;; 				(("*Paradox Report*" "*package update results*") :size 0.2 :align below :autoclose t)
;; 				("*Package-Lint*" :size 0.4 :align below :autoclose t)
;; 				("*How Do You*" :select t :size 0.5 :align below :autoclose t)
;; 				("*Geiser Guile REPL*" :size 0.3 :align below :autoclose t)
;; 				("*Geiser dbg*" :size 0.3 :align below :autoclose t)
;; 				(("*Org Agenda*" " *Agenda Commands*" " *Org todo*" "*Org Dashboard*" "*Org Select*") :select t :size 0.55 :align right :autoclose t)
;; 				(("\\*Capture\\*" "^CAPTURE-.*\\.org*") :regexp t :select t :size 0.3 :align below :autoclose t)

;; 				("*ert*" :size 15 :align below :autoclose t)
;; 				(overseer-buffer-mode :size 15 :align below :autoclose t)

;; 				(" *Flycheck checkers*" :select t :size 0.3 :align below :autoclose t)
;; 				((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
;; 				 :select t :size 0.25 :align below :autoclose t)

;; 				(("*lsp-help*" "*lsp session*") :size 0.3 :align below :autoclose t)
;; 				("*DAP Templates*" :select t :size 0.4 :align below :autoclose t)
;; 				(dap-server-log-mode :size 15 :align below :autoclose t)
;; 				("*rustfmt*" :select t :size 0.3 :align below :autoclose t)
;; 				((rustic-compilation-mode rustic-cargo-clippy-mode rustic-cargo-outdated-mode rustic-cargo-test-mode)
;; 				 :select t :size 0.3 :align below :autoclose t)

;; 				(profiler-report-mode :select t :size 0.5 :align below)
;; 				("*ELP Profiling Restuls*" :select t :size 0.5 :align below)

;; 				((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.3 :align below)
;; 				("*prolog*" :size 0.4 :align below)

;; 				(("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :align below :autoclose t)
;; 				(godoc-mode :select t :size 0.4 :align below :autoclose t)

;; 				((grep-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :size 0.4 :align below)
;; 				(Buffer-menu-mode :select t :size 20 :align below :autoclose t)
;; 				(gnus-article-mode :select t :size 0.7 :align below :autoclose t)
;; 				(magit-status-mode :select t :size 0.5 :align right :autoclose t)
;; 				(helpful-mode :select t :other t :size 0.4 :align below :autoclose t :volatile t)
;; 				((process-menu-mode cargo-process-mode) :select t :size 0.3 :align below :autoclose t)
;; 				(list-environment-mode :select t :size 0.3 :align below :autoclose t)
;; 				(geiser-doc-mode :select t :size 0.4 :align below)
;; 				("*sly-mrepl for sbcl*" :regexp t :select t :size 0.35 :align below)
;; 				("*sly-description*" :select t :size 0.3 :align below)
;; 				("*HTTP Response*" :select t :size 0.4 :align below)
;; 				(tabulated-list-mode :size 0.4 :align below)))))
