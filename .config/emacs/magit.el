;;; -*- lexical-binding: t -*-
(use-package magit
  :straight t)

;; --- Manage dotfiles via a bare repo in magit, like the shell alias
;; ---   git --git-dir=$HOME/.dotfiles --work-tree=$HOME
;; magit natively honours GIT_DIR/GIT_WORK_TREE (see `magit-toplevel' /
;; `magit-gitdir'), and both its sync and async git paths read the live
;; `process-environment'. So we set those two vars BUFFER-LOCALLY in the
;; dotfiles magit buffers only. Every dotfiles buffer (the status buffer and
;; the diff/log/commit/... children it spawns) is rooted at the work-tree
;; ($HOME), so keying on that scopes it to this session + its children without
;; any global state or effect on other repositories.

(defvar +magit-dotfiles-git-dir (expand-file-name "~/.dotfiles/")
  "Git directory of the dotfiles bare repo.")
(defvar +magit-dotfiles-work-tree (expand-file-name "~/")
  "Work tree of the dotfiles repo (your $HOME).")

(defun +magit-dotfiles-env ()
  "`process-environment' with GIT_DIR/GIT_WORK_TREE for the dotfiles repo.
Built from the global env (not the current, possibly already-augmented one) so
it is safe to call repeatedly without stacking duplicates."
  (append (list (concat "GIT_DIR=" +magit-dotfiles-git-dir)
                (concat "GIT_WORK_TREE=" +magit-dotfiles-work-tree))
          (default-value 'process-environment)))

(defun +magit-dotfiles-buffer-p ()
  "Non-nil if the current magit buffer belongs to the dotfiles session.
All dotfiles magit buffers (status + children) share the work-tree as their
`default-directory'."
  (and (file-directory-p +magit-dotfiles-git-dir)
       (file-equal-p default-directory +magit-dotfiles-work-tree)))

(defun +magit-dotfiles-setup-env ()
  "Make GIT_DIR/GIT_WORK_TREE buffer-local in dotfiles magit buffers.
Runs for every magit buffer (status and every child it spawns); other repos
are left alone."
  (when (+magit-dotfiles-buffer-p)
    (setq-local process-environment (+magit-dotfiles-env))))

(add-hook 'magit-mode-hook #'+magit-dotfiles-setup-env)

(defun +magit-dotfiles-status ()
  "Open magit on the dotfiles repo (the `dotfiles' alias, in Emacs)."
  (interactive)
  (let ((default-directory +magit-dotfiles-work-tree)
        ;; Needed for the initial repo detection, which happens before the
        ;; status buffer (and its `magit-mode-hook') exists.
        (process-environment (+magit-dotfiles-env)))
    (magit-status-setup-buffer default-directory)))
