;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Areg Hov"
	  user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 20 :weight 'semi-light)
	  doom-variable-pitch-font (font-spec :family "sans" :size 20))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(evil-ex-define-cmd "W" 'evil-write)

(evil-define-operator evil-delete-line-without-yank (beg end type reg yank-handler)
  :motion evil-line-or-visual-line
  (evil-delete-whole-line beg end type ?_ yank-handler))

(define-key evil-normal-state-map "x" 'delete-forward-char)
(define-key evil-normal-state-map "X" 'delete-backward-char)
(define-key evil-normal-state-map "C" 'evil-delete-line-without-yank)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)

(use-package! evil-snipe
  :config
  (setq evil-snipe-scope 'whole-buffer)
  )

(setq neo-theme 'icons)

(defun dired-up-alternate-file ()
  "In Dired, go up one directory, reusing the current buffer"
  (interactive)
  (find-alternate-file "..")
  )

(map! :after dired
	  :map dired-mode-map
	  :n "-"
	  'dired-up-alternate-file)

(map! :leader
	  :desc "Open calculator" "oc" 'calc)

(map! :leader
	  :desc "Previous buffer" "j" 'previous-buffer
	  :desc "Next buffer" "k" 'next-buffer)

(setq tab-width 4
	  evil-shift-width 4
	  lua-indent-level 4
	  indent-tabs-mode t)


(add-hook 'org-mode-hook 'hl-todo-mode)
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)
