;;; -*- lexical-binding: t -*-

(use-package agent-shell
  :straight t
  :hook (agent-shell-mode . mixed-pitch-mode)
  :config
  (setq
   agent-shell-anthropic-claude-environment
   (agent-shell-make-environment-variables
	"CLAUDE_CODE_EXECUTABLE" (executable-find "claude")
	:inherit-env t)

   agent-shell-mcp-servers
   '(((name . "emacs")
	  (command . "socat")
	  (args . ("-" "UNIX-CONNECT:/home/areg/.config/emacs/emacs-mcp-server.sock"))
	  (env . ())))))

(use-package mcp-server
  :straight (:type git :host github :repo "rhblind/emacs-mcp-server"
				   :files ("*.el" "tools/*.el" "mcp-wrapper.py" "mcp-wrapper.sh")))
