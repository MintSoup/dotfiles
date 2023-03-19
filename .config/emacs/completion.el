;;; -*- lexical-binding: t -*-
(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(defun consult-buffer-ignore-asterisks ()
  (interactive)
  (let ((consult-buffer-filter
		 '("\\` " "\\`\\*.*\\*\\'")))
	(consult-buffer)))

(use-package vertico
  :straight t
  :init
  (setq vertico-count 13
		vertico-resize 'fixed
		vertico-cycle nil)
  (vertico-mode)
  :config
  (general-define-key
   :keymaps 'minibuffer-mode-map
   "C-w" 'dw/minibuffer-backward-kill

   :keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous
   "C-M-j" 'vertico-exit-input))

(use-package orderless
  :straight t
  :init
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   orderless-matching-styles '(orderless-regexp)
   completion-category-overrides '((file (styles basic-remote orderless)))
   orderless-style-dispatchers
   (cl-macrolet
	   ((orderless-prefix-dispatcher (prefix style)
		  `(lambda (pattern index total)
			 (cond
			  ((equal ,prefix pattern)
			   '(orderless-literal . ""))
			  ((string-prefix-p ,prefix pattern)
			   (cons (quote ,style) (substring pattern 1)))))))
	 (list
	  (orderless-prefix-dispatcher "@" orderless-flex)
	  (orderless-prefix-dispatcher "$" orderless-prefixes)
	  (orderless-prefix-dispatcher "!" orderless-without-literal))))
  ;; TRAMP completion
  (defun basic-remote-try-completion (string table pred point)
	(and (vertico--remote-p string)
		 (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
	(and (vertico--remote-p string)
		 (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil)))

(use-package marginalia
  :straight t
  :after (vertico)
  :init
  (setq marginalia-align 'left
		marginalia-align-offset -3)
  (marginalia-mode))

(use-package consult
  :straight t
  :init
  (setq consult-buffer-sources
		'(consult--source-hidden-buffer
		  consult--source-modified-buffer
		  consult--source-buffer
		  consult--source-project-buffer)
		consult-project-function
		(lambda (d)
		  (project-current-root)))
  :config
  (consult-customize consult-ripgrep
					 consult-recent-file
					 :preview-key
					 (list (kbd "C-p"))))

(use-package all-the-icons-completion
  :straight (all-the-icons-completion
			 :type git :host github :repo "iyefrat/all-the-icons-completion"
			 :fork (:host github :repo "MintSoup/all-the-icons-completion"))
  :after (marginalia all-the-icons)
  :config
  (all-the-icons-completion-mode))
