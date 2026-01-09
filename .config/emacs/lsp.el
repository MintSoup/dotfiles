;;; -*- lexical-binding: t -*-

(use-package lsp-mode
  :straight t
  :hook (c-mode . lsp)
  :hook (c++-mode . lsp)
  :hook (rust-ts-mode . lsp)
  ;; :hook (java-mode . lsp)
  :config
  (setq lsp-idle-delay 0.1
		lsp-headerline-breadcrumb-enable nil
		lsp-eldoc-render-all t
		eldoc-echo-area-use-multiline-p 1
		lsp-signature-doc-lines 0)


  ;; (general-define-key :keymaps 'lsp-mode-map
  ;; 					  :states 'normal
  ;; 					  "K" 'eldoc)

  ;; TODO: Is this janky? It's supposed to replace the above commented code.
  (defun lsp-evil-hook ()
	(setq-local evil-lookup-func 'eldoc))
  (add-hook 'lsp-mode-hook 'lsp-evil-hook))

(use-package lsp-ui
  :after lsp-mode
  :straight t)


;; Use-package gives some weird error
(straight-use-package 'consult-lsp)

;; (use-package consult-lsp
;;   :straight t)
