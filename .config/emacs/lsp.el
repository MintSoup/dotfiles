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

;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   :straight t)

(use-package lsp-ui
  :after lsp-mode
  :straight t)

(use-package eglot
  :straight t
  ;; :hook (c-mode . eglot-ensure)
  ;; :hook (c++-mode . eglot-ensure)
  ;; :hook (c-ts-mode . eglot-ensure)
  ;; :hook (c++-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
			   '((c++-mode c-mode) "clangd" "--completion-style=detailed" "--header-insertion-decorators=0"))
  (setq eglot-send-changes-idle-time 0.1
		eldoc-idle-delay 0.2))

(use-package consult-lsp
  :straight t)
;; (use-package eglot-booster
;;   :after eglot
;;   :straight (:type git :host github :repo "jdtsmith/eglot-booster")
;;   :config	(eglot-booster-mode))

;; (use-package consult-eglot
;;   :straight t)
