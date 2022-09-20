;;; -*- lexical-binding: t -*-

(use-package flycheck
  :straight t
  :hook (lsp-mode . flycheck-mode))
