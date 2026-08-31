;;; -*- lexical-binding: t -*-
(use-package which-key
  :straight t
  :init
  (setq-default which-key-idle-delay 0.35
				which-key-idle-secondary-delay 0.000001
				which-key-side-window-max-height 0.1)
  :config
  (which-key-mode +1))
