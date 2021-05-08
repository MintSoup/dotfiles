;;; -*- lexical-binding: t -*-
(electric-pair-mode +1)
(global-visual-line-mode +1)
(recentf-mode +1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
