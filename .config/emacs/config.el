;;; -*- lexical-binding: t -*-

(electric-pair-mode +1)

(setq recentf-max-menu-items 100
	  recentf-max-saved-items 100)

(recentf-mode +1)
(global-visual-line-mode +1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'Info-mode-hook 'visual-fill-column-mode)
(add-hook 'shell-mode-hook 'evil-normal-state)

(setq css-fontify-colors nil)

(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
	(delete-trailing-whitespace)))

(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

(setq make-backup-files nil)
(setq backup-directory-alist
	  `(("." . ,(concat user-emacs-directory "backups/"))))


(add-hook 'imenu-after-jump-hook
		  (lambda ()
			(evil-scroll-line-to-top (line-number-at-pos))))

(use-package lorem-ipsum
  :straight t)

(use-package fish-mode
  :straight t)

(winner-mode)

(defun indent-buffer ()
  "Call `indent-region' on the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun tabify-buffer ()
  "Call `tabify' on the entire buffer."
  (interactive)
  (tabify (point-min) (point-max)))

(setq-default major-mode 'text-mode)


(with-eval-after-load 'tramp
  (setq tramp-default-method "ssh"
		;; To allow logging into non-standard
		;; systems, i.e. guix or nix where
		;; /bin/ls doesn't exist
		tramp-remote-path (append tramp-remote-path
								  '(tramp-own-remote-path))))

(setq sentence-end-double-space nil)


(use-package ztree
  :straight t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
	  ediff-split-window-function 'split-window-horizontally)

(setq epg-pinentry-mode 'loopback)

(setq gamegrid-glyph-height-mm 7.0)

(setq enable-recursive-minibuffers t)

(use-package ace-window
  :straight t
  :config
  (ace-window-display-mode))

(use-package wgrep
  :straight t
  :config
  (general-define-key :keymaps 'grep-mode
					  :states 'normal
					  "i" 'wgrep-change-to-wgrep-mode)
  (general-define-key :keymaps 'wgrep-mode-map
					  :states 'normal
					  "i" 'evil-cp-insert))

(general-define-key
 :keymaps '(c-mode-map c++-mode-map java-mode-map)
 :states '(normal insert)
 "C-<return>" 'c-indent-new-comment-line)

;; (defun +c-indent-new-comment-line (&optional soft allow-auto-fill)
;;   "Break line at point and indent, continuing comment or macro if within one.
;; If inside a comment and `comment-multi-line' is non-nil, the
;; indentation and line prefix are preserved (see the
;; `c-comment-prefix-regexp' and `c-block-comment-prefix' variables for
;; details).  If inside a single line comment and `comment-multi-line' is
;; nil, a new comment of the same type is started on the next line and
;; indented as appropriate for comments.  If inside a macro, a line
;; continuation backslash is inserted and aligned as appropriate, and the
;; new line is indented according to `c-syntactic-indentation'.

;; If a fill prefix is specified, it overrides all the above."
;;   ;; allow-auto-fill is used from c-context-line-break to allow auto
;;   ;; filling to break the line more than once.  Since this function is
;;   ;; used from auto-fill itself, that's normally disabled to avoid
;;   ;; unnecessary recursion.
;;   (interactive)
;;   (c-with-string-fences
;;    (let ((fill-prefix fill-prefix)
;; 		 (do-line-break
;; 		  (lambda ()
;; 			(delete-horizontal-space)
;; 			(if soft
;; 				(insert-and-inherit ?\n)
;; 			  (newline (if allow-auto-fill nil 1)))))
;; 		 ;; Already know the literal type and limits when called from
;; 		 ;; c-context-line-break.
;; 		 (c-lit-limits c-lit-limits)
;; 		 (c-lit-type c-lit-type)
;; 		 (c-macro-start c-macro-start))

;;      (c-save-buffer-state ()
;;        (when (not (eq c-auto-fill-prefix t))
;; 		 ;; Called from do-auto-fill.
;; 		 (unless c-lit-limits
;; 		   (setq c-lit-limits (c-literal-limits nil nil t)))
;; 		 (unless c-lit-type
;; 		   (setq c-lit-type (c-literal-type c-lit-limits)))
;; 		 (if (memq (cond ((c-query-and-set-macro-start) 'cpp)
;; 						 ((null c-lit-type) 'code)
;; 						 (t c-lit-type))
;; 				   c-ignore-auto-fill)
;; 			 (setq fill-prefix t)	; Used as flag in the cond.
;; 		   (if (and (null c-auto-fill-prefix)
;; 					(eq c-lit-type 'c)
;; 					(<= (c-point 'bol) (car c-lit-limits)))
;; 			   ;; The adaptive fill function has generated a prefix, but
;; 			   ;; we're on the first line in a block comment so it'll be
;; 			   ;; wrong.  Ignore it to guess a better one below.
;; 			   (setq fill-prefix nil)
;; 			 (when (and (eq c-lit-type 'c++)
;; 						(not (string-match (concat "\\`[ \t]*"
;; 												   c-line-comment-starter)
;; 										   (or fill-prefix ""))))
;; 			   ;; Kludge: If the function that adapted the fill prefix
;; 			   ;; doesn't produce the required comment starter for line
;; 			   ;; comments, then we ignore it.
;; 			   (setq fill-prefix nil)))
;; 		   )))

;;      (cond ((eq fill-prefix t)
;; 			;; A call from do-auto-fill which should be ignored.
;; 			)
;; 		   (fill-prefix
;; 			;; A fill-prefix overrides anything.
;; 			(funcall do-line-break)
;; 			(insert-and-inherit fill-prefix))
;; 		   ((c-save-buffer-state ()
;; 			  (unless c-lit-limits
;; 				(setq c-lit-limits (c-literal-limits)))
;; 			  (unless c-lit-type
;; 				(setq c-lit-type (c-literal-type c-lit-limits)))
;; 			  (memq c-lit-type '(c c++)))
;; 			;; Some sort of comment.
;; 			(if (or comment-multi-line
;; 					(save-excursion
;; 					  (goto-char (car c-lit-limits))
;; 					  (end-of-line)
;; 					  (< (point) (cdr c-lit-limits))))
;; 				;; Inside a comment that should be continued.
;; 				(let ((fill (c-save-buffer-state nil
;; 							  (c-guess-fill-prefix
;; 							   (setq c-lit-limits
;; 									 (c-collect-line-comments c-lit-limits))
;; 							   c-lit-type)))
;; 					  (pos (point))
;; 					  (comment-text-end
;; 					   (or (and (eq c-lit-type 'c)
;; 								(save-excursion
;; 								  (goto-char (- (cdr c-lit-limits) 2))
;; 								  (if (looking-at "\\*/") (point))))
;; 						   (cdr c-lit-limits))))
;; 				  ;; Skip forward past the fill prefix in case
;; 				  ;; we're standing in it.
;; 				  ;;
;; 				  ;; FIXME: This doesn't work well in cases like
;; 				  ;;
;; 				  ;; /* Bla bla bla bla bla
;; 				  ;;         bla bla
;; 				  ;;
;; 				  ;; If point is on the 'B' then the line will be
;; 				  ;; broken after "Bla b".
;; 				  ;;
;; 				  ;; If we have an empty comment, /*   */, the next
;; 				  ;; lot of code pushes point to the */.  We fix
;; 				  ;; this by never allowing point to end up to the
;; 				  ;; right of where it started.
;; 				  (while (and (< (current-column) (cdr fill))
;; 							  (not (eolp)))
;; 					(forward-char 1))
;; 				  (if (and (> (point) comment-text-end)
;; 						   (> (c-point 'bol) (car c-lit-limits)))
;; 					  (progn
;; 						;; The skip takes us out of the (block)
;; 						;; comment; insert the fill prefix at bol
;; 						;; instead and keep the position.
;; 						(setq pos (copy-marker pos t))
;; 						(beginning-of-line)
;; 						(insert-and-inherit (car fill))
;; 						(if soft (insert-and-inherit ?\n) (newline 1))
;; 						(goto-char pos)
;; 						(set-marker pos nil))
;; 					;; Don't break in the middle of a comment starter
;; 					;; or ender.
;; 					(cond ((> (point) comment-text-end)
;; 						   (goto-char comment-text-end))
;; 						  ((< (point) (+ (car c-lit-limits) 2))
;; 						   (goto-char (+ (car c-lit-limits) 2))))
;; 					(funcall do-line-break)
;; 					(insert-and-inherit (car fill))
;; 					(if (and (looking-at c-block-comment-ender-regexp)
;; 							 (memq (char-before) '(?\  ?\t)))
;; 						(backward-char)))) ; can this hit the
;; 										; middle of a TAB?
;; 			  ;; Inside a comment that should be broken.
;; 			  (let ((comment-start comment-start)
;; 					(comment-end comment-end)
;; 					col)
;; 				(if (eq c-lit-type 'c)
;; 					(unless (string-match "[ \t]*/\\*" comment-start)
;; 					  (setq comment-start "/* " comment-end " */"))
;; 				  (unless (string-match "[ \t]*//" comment-start)
;; 					(setq comment-start "// " comment-end "")))
;; 				(setq col (save-excursion
;; 							(back-to-indentation)
;; 							(current-column)))
;; 				(funcall do-line-break)
;; 				(when (and comment-end (not (equal comment-end "")))
;; 				  (forward-char -1)
;; 				  (insert-and-inherit comment-end)
;; 				  (forward-char 1))
;; 				;; c-comment-indent may look at the current
;; 				;; indentation, so let's start out with the same
;; 				;; indentation as the previous one.
;; 				(indent-to col)
;; 				(insert-and-inherit comment-start)
;; 				(indent-for-comment))))
;; 		   ((c-query-and-set-macro-start)
;; 			;; In a macro.
;; 			(unless (looking-at "[ \t]*\\\\$")
;; 			  ;; Do not clobber the alignment of the line continuation
;; 			  ;; slash; c-backslash-region might look at it.
;; 			  (delete-horizontal-space))
;; 			;; Got an asymmetry here: In normal code this command
;; 			;; doesn't indent the next line syntactically, and otoh a
;; 			;; normal syntactically indenting newline doesn't continue
;; 			;; the macro.
;; 			(c-newline-and-indent (if allow-auto-fill nil 1)))
;; 		   (t
;; 			;; Somewhere else in the code.
;; 			(let ((col (save-excursion
;; 						 (beginning-of-line)
;; 						 (while (and (looking-at "[ \t]*\\\\?$")
;; 									 (= (forward-line -1) 0)))
;; 						 (current-indentation))))
;; 			  ;; (funcall do-line-break)
;; 			  (newline 1 t)))))))

;; (advice-add 'c-indent-new-comment-line :override '+c-indent-new-comment-line)
