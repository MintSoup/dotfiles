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
  (general-define-key :keymaps 'vertico-map
					  "C-j" 'vertico-next
					  "C-k" 'vertico-previous
					  "C-w" 'dw/minibuffer-backward-kill
					  "C-M-j" 'vertico-exit-input))

(use-package orderless
  :straight t
  :init
  (setq
   completion-styles '(orderless basic)
   completion-category-defaults nil
   orderless-matching-styles '(orderless-regexp)
   orderless-style-dispatchers
   (list
	(lambda (pattern index total) ;; Flex~
	  (when (string-suffix-p "~" pattern)
		`(orderless-flex . ,(substring pattern 0 -1))))
	(lambda (pattern index total) ;; Prefix matcher
	  (when (string-suffix-p "\\" pattern)
		`(orderless-prefixes . ,(substring pattern 0 -1))))
	(lambda (pattern index total) ;; Without!
	  (cond
	   ((equal "!" pattern)
		'(orderless-literal . ""))
	   ((string-prefix-p "!" pattern)
		`(orderless-without-literal . ,(substring pattern 1))))))))


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
		  (projectile-project-root)))
  :config
  (consult-customize consult-ripgrep consult-recent-file :preview-key
					 (list (kbd "C-p"))))



(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :config
  (all-the-icons-completion-mode))
