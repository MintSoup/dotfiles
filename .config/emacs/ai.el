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

(use-package agent-shell-notifications
  :straight (agent-shell-notifications
             :type git
             :host github
             :repo "zackattackz/agent-shell-notifications")

  :hook
  (agent-shell-mode . agent-shell-notifications-mode)
  (agent-shell-viewport-edit-mode . agent-shell-notifications-viewport-edit-mode)
  (agent-shell-viewport-view-mode . agent-shell-notifications-viewport-view-mode)

  :config
  (setq agent-shell-notifications-timeout 5))
