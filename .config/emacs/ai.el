;;; -*- lexical-binding: t -*-

(use-package agent-shell
  :straight t
  :hook (agent-shell-mode . mixed-pitch-mode)
  :config
  (setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables
       "CLAUDE_CODE_EXECUTABLE" (executable-find "claude")
       :inherit-env t)))
