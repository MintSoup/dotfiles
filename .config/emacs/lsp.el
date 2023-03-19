;;; -*- lexical-binding: t -*-

(use-package lsp-mode
  :straight t
  :hook (c-mode . lsp)
  ;; :hook (java-mode . lsp)
  :init
  (setq lsp-idle-delay 0.1
		;; lsp-headerline-breadcrumb-enable t
		;; lsp-headerline-breadcrumb-icons-enable t
		;; lsp-headerline-breadcrumb-segments '(project file symbols))
  ))

(use-package lsp-treemacs
  :after lsp-mode
  :straight t)

(use-package lsp-ui
  :after lsp-mode
  :straight t)

;; (use-package eglot
;;   :straight t
;;   :hook (c-mode . eglot-ensure)
;;   :hook (c++-mode . eglot-ensure)
;;   :hook (c-ts-mode . eglot-ensure)
;;   :hook (c++-ts-mode . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs
;; 			   '((c++-mode c-mode) "clangd" "--completion-style=detailed" "--header-insertion-decorators=0"))
;;   (setq eglot-send-changes-idle-time 0.1
;; 		eldoc-idle-delay 0.1))

;; (use-package consult-eglot
;;   :straight t)
