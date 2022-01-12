;;; -*- lexical-binding: t -*-

(use-package lsp-mode
	:straight t
	:hook (c-mode . lsp)
	:init
	(setq lsp-idle-delay 0.1
		  lsp-headerline-breadcrumb-enable t
		  lsp-headerline-breadcrumb-icons-enable t
		  lsp-headerline-breadcrumb-segments '(project file symbols)
		  lsp-completion-provider :none))

(use-package lsp-treemacs
	:straight t)

(use-package lsp-ui
	:straight t
	:config
	(setq lsp-ui-doc-enable nil))

(use-package lsp-ivy
	:straight t)


;; (use-package lsp-python-ms
;; 	:straight t)
