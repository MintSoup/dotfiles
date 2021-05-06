;;; -*- lexical-binding: t -*-
(use-package helpful
	:straight t
	:init
	(advice-add 'describe-function :override 'helpful-function)
	(advice-add 'describe-key :override 'helpful-key)
	(advice-add 'describe-variable :override 'helpful-variable))
