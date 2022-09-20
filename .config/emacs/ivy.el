;;; -*- lexical-binding: t -*-
(use-package ivy
  :straight t
  :defer 0.1
  :diminish
  :init
  (setq ivy-height 15
		ivy-initial-inputs-alist nil
		ivy-on-del-error-function #'ignore
		ivy-re-builders-alist '((t . ivy--regex-ignore-order))
		enable-recursive-minibuffers t)
  :config
  (general-define-key
   :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line
   "C-h" 'evil-backward-char
   "C-l" 'evil-forward-char
   "C-w" 'ivy-backward-kill-word
   "M-i" 'ivy-insert-current
   "C-<return>" '+ivy-toggle-mark)

  (general-define-key
   :keymaps 'minibuffer-mode-map
   "C-h" 'evil-backward-char
   "C-l" 'evil-forward-char
   "C-w" 'ivy-backward-kill-word)

  (ivy-mode))


(defun +ivy-toggle-mark ()
  "Toggle the mark of the selected candidate and move to the next one.

In `ivy-call', :action will be called in turn for all marked
candidates.

However, if :multi-action was supplied to `ivy-read', then it
will be called with `ivy-marked-candidates'. This way, it can
make decisions based on the whole marked list."
  (interactive)
  (if (ivy--marked-p)
	  (ivy--unmark (ivy-state-current ivy-last))
	(ivy--mark (ivy-state-current ivy-last)))
  (ivy-next-line))


(defun +counsel-switch-buffer-hide-asterisk ()
  (interactive)
  (let ((ivy-ignore-buffers '("\\` " "\\`\\*")))
	(counsel-switch-buffer)))

(use-package counsel
  :straight t
  :after ivy
  :config (counsel-mode +1))

(use-package counsel-fd
  :straight t
  :after counsel)

(use-package all-the-icons-ivy-rich
  :straight t
  :after counsel-projectile
  :init
  (setq all-the-icons-ivy-rich-project t)
  :config
  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  :config
  (defun make-cacher-function (oldfn &optional keep)
	(let ((hashtable (make-hash-table :test 'equal)))
	  (unless keep
		(add-hook 'minibuffer-mode-hook
				  (lambda ()
					(clrhash hashtable))))
	  (lambda (delegate candidate)
		"Cache the results for this function to speed things up"
		(let ((result (gethash candidate hashtable)))
		  (unless result
			(setq result (funcall delegate candidate))
			(puthash candidate result hashtable))
		  result))))

  (advice-add 'all-the-icons-ivy-rich--project-file-path :around
			  (make-cacher-function 'all-the-icons-ivy-rich--project-file-path t))

  (advice-add 'all-the-icons-ivy-rich-file-modes :around
			  (make-cacher-function 'all-the-icons-ivy-rich-modes))
  (advice-add 'all-the-icons-ivy-rich-file-id :around
			  (make-cacher-function 'all-the-icons-ivy-rich-file-id))
  (advice-add 'all-the-icons-ivy-rich-file-size :around
			  (make-cacher-function 'all-the-icons-ivy-rich-file-size))
  (advice-add 'all-the-icons-ivy-rich-file-modification-time :around
			  (make-cacher-function 'all-the-icons-ivy-rich-file-modification-time))
  (advice-add 'all-the-icons-ivy-rich-project-file-modes :around
			  (make-cacher-function 'all-the-icons-ivy-rich-project-file-modes))
  (advice-add 'all-the-icons-ivy-rich-project-file-id :around
			  (make-cacher-function 'all-the-icons-ivy-rich-project-file-id))
  (advice-add 'all-the-icons-ivy-rich-project-file-size :around
			  (make-cacher-function 'all-the-icons-ivy-rich-project-file-size))
  (advice-add 'all-the-icons-ivy-rich-project-file-modification-time :around
			  (make-cacher-function 'all-the-icons-ivy-rich-project-file-modification-time))
  (advice-add 'counsel-projectile-find-file-transformer :around
			  (make-cacher-function 'counsel-projectile-find-file-transformer))
  (advice-add 'ivy-rich--all-the-icons-ivy-rich-kill-buffer-transformer :around
			  (make-cacher-function 'ivy-rich--all-the-icons-ivy-rich-kill-buffer-transformer))
  (advice-add 'ivy-rich--ivy-switch-buffer-transformer :around
			  (make-cacher-function 'ivy-rich--ivy-switch-buffer-transformer)))

(use-package ivy-rich
  :straight t
  :after all-the-icons-ivy-rich
  :init
  (setq ivy-rich-path-style 'abbrev
		ivy-virtual-abbreviate 'full))

(use-package swiper
  :straight t
  :after ivy)
