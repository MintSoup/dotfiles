;;; -*- lexical-binding: t -*-

(use-package beardbolt
  :straight (beardbolt :type git :host github
					   :repo "joaotavora/beardbolt")
  :init
  (setq beardbolt-asm-format 'intel))
