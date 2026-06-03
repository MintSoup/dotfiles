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
		lsp-signature-doc-lines 0
		lsp-completion-provider :capf ; use capf (corfu), never company
		lsp-enable-on-type-formatting nil ; don't reformat as I type (cursor/indent jumps)
		lsp-enable-indentation t
		lsp-enable-snippet nil)           ; no server-snippet (yasnippet) expansion on completion


  ;; (general-define-key :keymaps 'lsp-mode-map
  ;; 					  :states 'normal
  ;; 					  "K" 'eldoc)

  ;; TODO: Is this janky? It's supposed to replace the above commented code.
  (defun lsp-evil-hook ()
	(setq-local evil-lookup-func 'eldoc))
  (add-hook 'lsp-mode-hook 'lsp-evil-hook)

  ;; Use orderless (not lsp's `lsp-passthrough') for lsp completion, so orderless
  ;; filtering (M-SPC multi-component) and `orderless-match-face-N' highlighting
  ;; apply instead of clangd-side filtering + `completions-common-part'.
  (defun +lsp-orderless-setup ()
    (setf (alist-get 'lsp-capf completion-category-defaults)
          '((styles orderless basic)))
    ;; keep clangd's semantic ranking (don't let corfu re-sort by prefix/length)
    (setq-local corfu-sort-function nil))
  (add-hook 'lsp-completion-mode-hook #'+lsp-orderless-setup))

(use-package lsp-ui
  :after lsp-mode
  :straight t)


;; Use-package gives some weird error
(straight-use-package 'consult-lsp)

;; (use-package consult-lsp
;;   :straight t)
