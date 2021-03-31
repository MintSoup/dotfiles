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
(setq doom-theme 'my-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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
      :n "C-S-A" 'evil-numbers/dec-at-pt
      :n "Ö" 'evil-ex
      :n "°" 'evil-invert-char)

(map! :after evil-org
      :map evil-org-mode-map
      :n "x" 'evil-org-delete-char-without-yank)

(use-package! evil-snipe
  :config
  (setq evil-snipe-scope 'whole-buffer)
  (setq evil-snipe-repeat-scope 'whole-buffer))

(defun dired-up-alternate-file ()
  "In Dired, go up one directory, reusing the current buffer"
  (interactive)
  (find-alternate-file ".."))

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
      indent-tabs-mode t)

(add-hook 'org-mode-hook 'hl-todo-mode)
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)
(setq which-key-idle-delay 0.35)
