;;; -*- lexical-binding: t -*-
(use-package format-all
	:straight t
	:init
	(setq format-all-formatters
		  '(("C" clang-format))))
