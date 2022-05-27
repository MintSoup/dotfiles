;;; -*- lexical-binding: t -*-
;; (use-package nasm-mode
;; 	:straight t)

(use-package gas-mode
	:straight t)
(add-to-list 'auto-mode-alist '("\\.nasm\\'" . nasm-mode))
