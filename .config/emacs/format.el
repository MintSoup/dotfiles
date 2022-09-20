;;; -*- lexical-binding: t -*-
(use-package apheleia
  :straight t)

(setf (alist-get 'java-mode apheleia-mode-alist) 'clang-format)
