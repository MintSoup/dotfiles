;;; -*- lexical-binding: t -*-

(use-package lsp-mode
	:straight t
	:hook (c-mode . lsp)
	:hook (java-mode . lsp)
	:init
	(setq lsp-idle-delay 0.1
		  lsp-headerline-breadcrumb-enable t
		  lsp-headerline-breadcrumb-icons-enable t
		  lsp-headerline-breadcrumb-segments '(project file symbols)
		  lsp-completion-provider :none))

(use-package lsp-treemacs
	:after lsp-mode
	:straight t)

(use-package lsp-ui
	:after lsp-mode
	:straight t)

(use-package lsp-ivy
	:after lsp-mode
	:straight t)

;; (use-package lsp-python-ms
;; 	:straight t)
