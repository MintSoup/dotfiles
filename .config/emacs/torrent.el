;;; -*- lexical-binding: t -*-
(use-package transmission
	:straight t
	:init
	(setq transmission-refresh-interval 0.25
		  transmission-refresh-modes
		  '(transmission-mode
			transmission-files-mode
			transmission-info-mode
			transmission-peers-mode)))
