;;; -*- lexical-binding: t -*-
;; General
(use-package general
	:straight t
	:config
	(general-evil-setup t))

(general-define-key :states '(normal visual motion) "SPC" nil)

(general-define-key :states 'normal :keymaps 'evil-snipe-mode-map "S" 'evil-avy-goto-char-2)

(general-define-key "<escape>" 'keyboard-escape-quit)

(general-define-key :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
					"C-k" 'ivy-previous-line
					"C-j" 'ivy-next-line
					"C-h" 'evil-backward-char
					"C-l" 'evil-forward-char
					"C-w" 'evil-delete-backward-word)

(general-define-key :states 'normal
					"C" 'evil-delete-line-without-yank
					"x" 'delete-forward-char
					"C-a" 'evil-numbers/inc-at-pt
					"C-S-a" 'evil-numbers/dec-at-pt)

(general-define-key :keymaps 'minibuffer-mode-map
					"C-h" 'evil-backward-char
					"C-l" 'evil-forward-char
					"C-w" 'evil-delete-backward-word)


(general-create-definer my-leader
	:keymaps 'override
	:states '(normal visual motion)
	:prefix "SPC")

(my-leader
	"j" '(my-next-buffer :wk "Next Buffer")
	"k" '(my-previous-buffer :wk "Previous buffer")
	"," '(switch-to-buffer :wk "Switch buffer")
	"." '(find-file :wk "Find file")
	";" '(eval-expression :wk "Eval expression")
	"SPC" '(counsel-projectile-find-file :wk "Find file in project"))

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
	"F" '(counsel-describe-face :wk "Describe face")
	"q" '(kill-emacs :wk "Kill Emacs")
	"i" '(info :wk "Emacs help")
	"r" '(my-reload-private-config :wk "Reload private config")
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
	"J" '(evil-window-move-far-down :wk "Move window to the far bottom")
	"K" '(evil-window-move-far-up :wk "Move window to the far top")
	"v" '(evil-window-vsplit :wk "Vertical split")
	"s" '(evil-window-split :wk "Horizontal split")
	"=" '(balance-windows :wk "Balance windows")
	"q" '(evil-quit :wk "Close window")
	"Q" '(kill-buffer-and-window :wk "Kill window and buffer"))


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
	"n" '(evil-buffer-new :wk "New buffer")
	"d" '(kill-current-buffer :wk "Kill buffer")
	"c" '(clean-buffers :wk "Clean buffers")
	"i" '(ibuffer :wk "IBuffer")
	"r" '(revert-buffer :wk "Revert buffer"))

(general-create-definer my-open-leader
	:keymaps 'override
	:states '(normal visual motion)
	:prefix "SPC o")

(my-open-leader
	"" '(:ignore t :wk "Open")
	"d" '(dired-jump :wk "Dired")
	"c" '(calc :wk "Calculator")
	"t" '(vterm-other-window :wk "VTerm")
	"T" '(vterm :wk "VTerm here")
	"r" '(ielm :wk "IELM")
	"e" '(eshell :wk "EShell"))

(general-create-definer my-file-leader
	:keymaps 'override
	:states '(normal visual motion)
	:prefix "SPC f")

(defun browse-my-config ()
	"Browse file in emacs config directory"
	(interactive)
	(counsel-find-file user-emacs-directory))

(my-file-leader

	"" '(:ignore t :wk "File")
	"s" '(save-buffer :wk "Save")
	"p" '(browse-my-config :wk "Find file in private config")
	"r" '(counsel-recentf :wk "Recent files"))

(general-create-definer my-local-leader
	:states '(normal visual motion)
	:prefix "SPC m")
(my-local-leader :states '(normal visual motion) "" '(:ignore t :wk "Local"))

(general-create-definer my-code-leader
	:keymaps 'override
	:states '(normal visual motion)
	:prefix "SPC c")

(my-code-leader
	"" '(:ignore t :wk "Code")
	"f" '(format-all-buffer :wk "Format buffer"))

(general-create-definer my-search-leader
	:states '(normal visual motion)
	:prefix "SPC s")

(my-search-leader
	"" '(:ignore t :wk "Search")
	"s" '(swiper :wk "Buffer")
	"S" '(swiper-all :wk "All open buffers")
	"d" '(counsel-locate :wk "Locate file")
	"r" '(counsel-rg :wk "Ripgrep")
	"i" '(counsel-imenu :wk "Symbol"))

(general-create-definer my-project-leader
	:states '(normal visual motion)
	:prefix "SPC p")

(my-project-leader
	"" '(:ignore t :wk "Project")
	"p" '(projectile-switch-project :wk "Open")
	"a" '(projectile-add-known-project :wk "Add"))
