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
	(ivy-mode))

(use-package counsel
	:straight t
	:after ivy
	:config (counsel-mode +1))

(use-package ivy-rich
	:straight t
	:after counsel
	:init
	(setq ivy-rich-path-style 'abbrev
          ivy-virtual-abbreviate 'full)
	:config
	(ivy-rich-mode +1)) ;; this gets us descriptions in M-x.

(use-package swiper
	:straight t
	:after ivy)
