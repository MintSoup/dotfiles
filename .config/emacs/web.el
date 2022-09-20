;;; -*- lexical-binding: t -*-

(my-local-leader :keymaps 'mhtml-mode-map
  "d" 'sgml-delete-tag
  "t" 'sgml-tag
  "c" 'sgml-close-tag)

(add-hook 'sgml-mode-hook 'sgml-electric-tag-pair-mode)

(advice-add 'sgml-delete-tag :after
			(lambda (arg)
			  (indent-buffer)))
(advice-add 'sgml-tag :after
			(lambda (arg arg2)
			  (indent-buffer)))
