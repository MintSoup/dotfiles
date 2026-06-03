;;; -*- lexical-binding: t -*-
(use-package magit
  :straight t)

;; --- Manage dotfiles via a bare repo in magit, like the shell alias
;; ---   git --git-dir=$HOME/.dotfiles --work-tree=$HOME
;; magit honours GIT_DIR/GIT_WORK_TREE and funnels every git invocation through
;; `magit-process-environment'. We inject those two vars there, but only for the
;; dotfiles repo and only when intended:
;;   * at the work-tree ($HOME) root -- so a *fresh* `magit'/`magit-log'/`magit-diff'
;;     etc. resolves dotfiles ONLY when started from $HOME, never from a random
;;     subdirectory; and
;;   * during a `magit-find-file' content fetch, which legitimately runs git from
;;     the file's own subdirectory (flagged via `+magit-dotfiles-active').
;; The "no ordinary .git here" guard means real repos nested under $HOME are never
;; shadowed. No global state; other repos/sessions unaffected. The
;; `+magit-dotfiles-status' wrapper opens the repo from anywhere (it runs at $HOME).

(defvar +magit-dotfiles-git-dir (expand-file-name "~/.dotfiles/")
  "Git directory of the dotfiles bare repo.")
(defvar +magit-dotfiles-work-tree (expand-file-name "~/")
  "Work tree of the dotfiles repo (your $HOME).")
(defvar +magit-dotfiles-active nil
  "Non-nil while a dotfiles operation legitimately runs git from a subdirectory.
Lets `magit-find-file' (whose content fetch runs in the file's own subdir) reach
the dotfiles repo, without making fresh `magit' starts in subdirs do so.")

(defun +magit-dotfiles-augment-env (env)
  "Prepend dotfiles GIT_DIR/GIT_WORK_TREE to ENV for the dotfiles repo.
Active only under the work-tree, where no ordinary .git claims the directory, and
either at the work-tree root (a deliberate start point) or during a flagged
dotfiles operation (`+magit-dotfiles-active').  Advice for
`magit-process-environment'."
  (if (and (file-directory-p +magit-dotfiles-git-dir)
           (file-in-directory-p default-directory +magit-dotfiles-work-tree)
           (not (locate-dominating-file default-directory ".git"))
           (or (file-equal-p default-directory +magit-dotfiles-work-tree)
               +magit-dotfiles-active))
      (append (list (concat "GIT_DIR=" +magit-dotfiles-git-dir)
                    (concat "GIT_WORK_TREE=" +magit-dotfiles-work-tree))
              env)
    env))

(advice-add 'magit-process-environment :filter-return #'+magit-dotfiles-augment-env)

;; `magit-find-file' fetches blob content with `default-directory' set to the
;; file's subdirectory; flag the operation as dotfiles when the calling context's
;; toplevel is the work-tree, so that subdir fetch is allowed through the gate.
(defun +magit-find-file-activate-dotfiles (orig rev file &rest args)
  (let ((+magit-dotfiles-active
         (and (file-directory-p +magit-dotfiles-git-dir)
              (let ((tl (ignore-errors (magit-toplevel))))
                (and tl (file-equal-p tl +magit-dotfiles-work-tree))))))
    (apply orig rev file args)))
(advice-add 'magit-find-file-noselect :around #'+magit-find-file-activate-dotfiles)

(defun +magit-dotfiles-status ()
  "Open magit on the dotfiles repo (the `dotfiles' alias, in Emacs)."
  (interactive)
  (let ((default-directory +magit-dotfiles-work-tree))
    (magit-status-setup-buffer default-directory)))
