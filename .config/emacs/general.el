;;; -*- (lexical-binding:) t -*-
;; General
(use-package general
  :straight t
  :config
  (general-evil-setup t))

(general-define-key :states '(normal visual motion) "SPC" nil)

(general-define-key "<escape>" 'keyboard-escape-quit)
(general-define-key :keymaps 'Info-mode-map :states 'normal "RET" 'Info-follow-nearest-node)
(general-define-key
 :keymaps 'override
 :states '(normal visual insert)
 "C-M-u" 'universal-argument)


(general-create-definer my-leader
  :keymaps 'override
  :states '(normal visual motion)
  :prefix "SPC")

(my-leader
  "j" '(my-next-buffer :wk "Next Buffer")
  "k" '(my-previous-buffer :wk "Previous buffer")
  "," '(consult-buffer-ignore-asterisks :wk "Switch buffer")
  "<" '(consult-buffer :wk "Switch buffer")
  "." '(find-file :wk "Find file")
  ";" '(eval-expression :wk "Eval expression")
  "g" '(magit :wk "Magit")
  "/" '(switch-to-buffer-other-window :wk "Switch to buffer in other window")
  "SPC" '(project-find-file :wk "Find file in project"))

(general-create-definer my-help-leader
  :keymaps 'override
  :states '(normal visual motion)
  :prefix "SPC h")

(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun my-reload-private-config ()
  "Reload private configuration"
  (interactive)
  (load-file user-init-file))

(my-help-leader
  "" '(:ignore t :wk "Help/Session")
  "k" '(describe-key :wk "Describe key")
  "f" '(describe-function :wk "Describe function")
  "v" '(describe-variable :wk "Describe variable")
  "o" '(describe-symbol :wk "Describe symbol")
  "F" '(describe-face :wk "Describe face")
  "q" '(kill-emacs :wk "Kill Emacs")
  "i" '(info :wk "Emacs help")
  "r" '(my-reload-private-config :wk "Reload private config")
  "l" '(set-input-method :wk "Change language")
  ;; "s" '(counsel-info-lookup-symbol :wk "Lookup symbol in texinfo")
  "c" '(my-edit-configuration :wk "Open init.el"))

(general-create-definer my-window-leader
  :keymaps 'override
  :states '(normal visual motion)
  :prefix "SPC w")

(my-window-leader
  "" '(:ignore t :wk "Window")
  "h" '(evil-window-left :wk "Switch to left window")
  "l" '(evil-window-right :wk "Switch to right window")
  "j" '(evil-window-down :wk "Switch to lower window")
  "k" '(evil-window-up :wk "Switch to upper window")
  "H" '(evil-window-move-far-left :wk "Move window to the left")
  "L" '(evil-window-move-far-right :wk "Move window to the right")
  "J" '(evil-window-move-very-bottom :wk "Move window to the far bottom")
  "K" '(evil-window-move-very-top :wk "Move window to the far top")
  "v" '(evil-window-vsplit :wk "Vertical split")
  "s" '(evil-window-split :wk "Horizontal split")
  "=" '(balance-windows :wk "Balance windows")
  "q" '(evil-quit :wk "Close window")
  "w" '(ace-window :wk "Ace window"))


(general-create-definer my-buffer-leader
  :keymaps 'override
  :states '(normal visual motion)
  :prefix "SPC b")

(defun clean-buffers ()
  "Delete all buffers."
  (interactive)
  (mapc 'kill-buffer
		(delq (get-buffer "*dashboard*")
			  (buffer-list))))

(my-buffer-leader
  "" '(:ignore t :wk "Buffer")
  "d" '(+kill-current-buffer :wk "Kill buffer")
  "D" '(kill-buffer-and-window :wk "Kill buffer and window")
  "c" '(clone-buffer :wk "Clone buffer")
  "k" '(clean-buffers :wk "Clean buffers")
  "i" '(ibuffer :wk "IBuffer")
  "v" '(visual-fill-column-mode :wk "Center text")
  "R" '(rename-buffer :wk "Rename buffer")
  "r" '(revert-buffer :wk "Revert buffer"))

(general-create-definer my-open-leader
  :keymaps 'override
  :states '(normal visual motion)
  :prefix "SPC o")

(defun +treemacs ()
  (interactive)
  (pcase (treemacs-current-visibility)
	('visible (delete-window (treemacs-get-local-window)))
	((or 'none 'exists)
	 (let ((project (project-current-root)))
	   (treemacs-do-add-project-to-workspace
		"/tmp"
		"cringe")
	   (treemacs-display-current-project-exclusively))
	 (with-current-buffer (treemacs-get-local-buffer)
	   (doom-modeline-mode +1)))))


(my-open-leader
  "" '(:ignore t :wk "Open")
  "d" '(project-dired :wk "Dired")
  "D" '(dired-jump :wk "Dired here")
  "c" '(calc :wk "Calculator")
  "v" '(project-vterm :wk "VTerm")
  "t" '(transmission :wk "Torrents")
  "r" '(ielm :wk "IELM")
  "p" '(+treemacs :wk "Toggle tree")
  "a" '(org-agenda :wk "Org Agenda")
  "e" '(project-eshell :wk "Eshell")
  "E" '(eshell :wk "Eshell here"))

(general-create-definer my-file-leader
  :keymaps 'override
  :states '(normal visual motion)
  :prefix "SPC f")

(defun browse-my-config ()
  "Browse file in emacs config directory"
  (interactive)
  (let ((default-directory user-emacs-directory))
	(call-interactively #'find-file)))

(defun browse-org-files ()
  "Browse file in emacs config directory"
  (interactive)
  (let ((default-directory "~/org/"))
	(call-interactively #'find-file)))

(my-file-leader
  "" '(:ignore t :wk "File")
  "s" '(save-buffer :wk "Save")
  "o" '(browse-org-files :wk "Browse Org")
  "a" '(find-alternate-file :wk "Find alternate file")
  "p" '(browse-my-config :wk "Browse private config")
  "r" '(consult-recent-file :wk "Recent files"))

(general-create-definer my-local-leader
  :states '(normal visual motion)
  :prefix "SPC m")

(my-local-leader "" '(:ignore t :wk "Local"))

(general-create-definer my-code-leader
  :keymaps 'override
  :states '(normal visual motion)
  :prefix "SPC c")

(my-code-leader
  "" '(:ignore t :wk "Code")
  "f" '(apheleia-format-buffer :wk "Format buffer")
  "r" '(lsp-rename :wk "Rename")
  "a" '(lsp-execute-code-action :wk "Fix")
  "n" '(flycheck-next-error :wk "Next Error")
  "p" '(flycheck-previous-error :wk "Previous Error")
  "e" '(+quickrun :wk "Execute")
  "E" '(quickrun-shell :wk "Execute in Eshell")
  ;; "b" '(lsp-ivy-workspace-symbol :wk "Browse")
  )

(general-create-definer my-search-leader
  :states '(normal visual motion)
  :prefix "SPC s")

(my-search-leader
  "" '(:ignore t :wk "Search")
  "s" '(consult-line :wk "Buffer")
  "S" '(consult-line-multi :wk "All open buffers")
  ;; "d" '(counsel-fd-dired-jump :wk "Find directory")
  ;; "f" '(counsel-fd-file-jump :wk "Find file")
  "o" '(consult-outline :wk "Outline")
  "r" '(consult-ripgrep :wk "Ripgrep")
  "i" '(consult-imenu :wk "Symbol"))

(general-create-definer my-project-leader
  :states '(normal visual motion)
  :prefix "SPC p")

(defun +project-stop-debug ()
  (interactive)
  (let ((buff (gdb-get-buffer 'gdbmi)))
	(when buff
	  (kill-buffer buff))))


(my-project-leader
  "" '(:ignore t :wk "Project")
  "p" '(project-switch-project :wk "Open")
  "d" '(+project-debug :wk "Debug")
  "D" '(+project-stop-debug :wk "Stop debug")
  ;; "i" '(projectile-invalidate-cache :wk "Invalidate cache")
  "c" '(project-compile :wk "Compile project")
  "r" '(+project-run :wk "Run project"))


(general-create-definer my-mode-leader
  :states '(normal visual motion)
  :prefix "SPC M"
  :keymaps 'override)

(my-mode-leader
  "o" '(org-mode :wk "Org")
  "e" '(emacs-lisp-mode :wk "Elisp")
  "c" '(c-mode :wk "C")
  "t" '(text-mode :wk "Text")
  "s" '(scheme-mode :wk "Scheme")
  "r" '(restclient-mode :wk "REST client")
  "j" '(java-mode :wk "Java")
  "l" '(lisp-mode :wk "Common Lisp")
  "p" '(python-mode :wk "Python"))


(general-create-definer my-insert-leader
  :states '(normal visual motion)
  :prefix "SPC i"
  :keymaps 'override)

(defun prompt-for-path (dir)
  "Prompt for directory and insert it."
  (interactive "F")
  (insert dir))

(my-insert-leader
  "" '(:ignore t :wk "Insert")
  "p" '(prompt-for-path :wk "Path")
  "c" '(insert-char :wk "Character")

  "l" '(:ignore t :wk "Lorem")
  "lp" '(lorem-ipsum-insert-paragraphs :wk "Paragraphs")
  "ls" '(lorem-ipsum-insert-sentences :wk "Sentences")
  "ll" '(lorem-ipsum-insert-list :wk "List"))
