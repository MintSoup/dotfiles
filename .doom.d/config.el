;; Splash
(setq fancy-splash-image (concat doom-private-dir "emacs.svg"))

;; Font and Theme
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 20 :weight 'semi-light)
	  doom-variable-pitch-font (font-spec :family "sans" :size 21)
	  doom-themes-enable-bold t
	  doom-themes-enable-italic t
	  doom-theme 'my-dark

	  doom-modeline-major-mode-icon t
	  doom-themes-treemacs-theme "doom-colors"
	  treemacs-width 30
	  treemacs--width-is-locked nil)

;; Org
(setq org-directory "~/Org/"
	  org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'hl-todo-mode)

;; Line numbers
(setq display-line-numbers-type t)

;; Evil customization
(evil-ex-define-cmd "W" 'evil-write)

(evil-define-operator evil-delete-line-without-yank (beg end type reg yank-handler)
	"Delete line without yanking."
	:motion evil-line-or-visual-line
	(evil-delete-whole-line beg end type ?_ yank-handler))

(evil-define-operator evil-org-delete-char-without-yank (count beg end type register)
	"Same as evil-line-or-visual-line but without yank."
	:motion evil-forward-char
	(interactive "p<R><x>")
	(evil-org-delete-char count beg end type ?_))

(map! :n "x" 'delete-forward-char
	  :n "C" 'evil-delete-line-without-yank
	  :n "C-a" 'evil-numbers/inc-at-pt
	  :n "C-S-a" 'evil-numbers/dec-at-pt)

(map! :after evil-org
	  :map evil-org-mode-map
	  :n "x" 'evil-org-delete-char-without-yank)

(map! :after evil-snipe
	  :map evil-snipe-mode-map
	  :n "S" 'evil-avy-goto-char-2)

(setq evil-snipe-scope 'whole-buffer
	  evil-snipe-repeat-scope 'whole-buffer)

;; Dired
(map! :after dired
	  :map dired-mode-map

	  :n "-"
	  (cmd! () (find-alternate-file ".."))

	  :n "Y"
	  (cmd! () (dired-copy-filename-as-kill 0))

	  :localleader
	  :n "m"
	  'dired-hide-dotfiles-mode)

(add-hook 'dired-mode-hook 'dired-hide-dotfiles-mode)

;; Custom keybinds
(map! :leader
	  :desc "Open calculator" "oc" 'calc)

(map! :leader
	  :desc "Previous buffer" "j" 'previous-buffer
	  :desc "Next buffer" "k" 'next-buffer)

(map! :i "TAB" 'tab-to-tab-stop)


;; Undo
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

;; Which key delay
(setq-default which-key-idle-delay 0.35
			  which-key-idle-secondary-delay 0.000001)

;; Indentation
(add-hook 'prog-mode-hook
		  (lambda ()
			  (setq indent-tabs-mode t)
			  (doom/set-indent-width 4)))
(add-hook 'emacs-lisp-mode-hook (lambda () (doom/set-indent-width 4)))
(setq lisp-body-indent 4)

(setq backward-delete-char-untabify-method nil)

;; Guile
(setq geiser-active-implementations '(guile))

;; Dashboard
(add-hook '+doom-dashboard-mode-hook 'hide-mode-line-mode)
