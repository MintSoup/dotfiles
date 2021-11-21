;;; -*- lexical-binding: t -*-
(use-package transmission
	:straight t
	:hook (transmission-mode . hl-line-mode)
	:init
	(setq transmission-refresh-interval 0.25
		  transmission-refresh-modes
		  '(transmission-mode
			transmission-files-mode
			transmission-info-mode
			transmission-peers-mode)))
