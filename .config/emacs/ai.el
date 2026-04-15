;;; -*- lexical-binding: t -*-
(use-package claude-code-ide
  :straight t
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(use-package agent-shell
  :straight t
  :config
  (setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables
       "CLAUDE_CODE_EXECUTABLE" (executable-find "claude")
       :inherit-env t))
  :hook
  (agent-shell-mode . company-mode))
