;;; -*- lexical-binding: t -*-

;; NOTE: corfu 2.10 needs compat >= 31 (it calls `set-local', an Emacs 31
;; function backported in compat-31.el). straight pulls compat in automatically
;; as corfu's dependency, but it resolves deps by NAME ONLY and ignores version
;; constraints -- so an older compat that is already built won't be upgraded.
;; Keep it current with `straight-pull-package' and pin via `straight-freeze-versions'.

(use-package corfu
  :straight t
  :init
  (setq corfu-auto t
        corfu-auto-delay 0	; mirror old company-idle-delay
        corfu-auto-prefix 1	; mirror old company-minimum-prefix-length
        corfu-cycle t
        corfu-preselect 'prompt		 ; tab-and-go: nothing preselected
        corfu-preview-current 'insert ; tab-and-go: preview current inline
        ;; If ESC mid-completion ever leaves stray previewed text under evil,
        ;; set `corfu-preview-current' to nil.
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-on-exact-match nil
        corfu-popupinfo-delay 0.5
        ;; keep the popup a constant width (default 15..100 makes it jump)
        corfu-min-width 60
        corfu-max-width 60)
  (add-hook 'prog-mode-hook #'corfu-mode)
  :config
  (require 'corfu-popupinfo)
  (require 'corfu-history)
  (corfu-popupinfo-mode +1)             ; doc popup beside candidate
  ;; `my-dark' styles `corfu-current' almost like the popup bg, so the selected
  ;; row is invisible — inherit `region' instead. Inherit (not a copied colour)
  ;; resolves per-frame, so it works under --daemon where `region' has no colour
  ;; at init time.
  (set-face-attribute 'corfu-current nil :inherit 'region :background 'unspecified)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))
  ;; Search among the currently offered candidates: ship them to the minibuffer
  ;; where vertico + orderless can filter them.
  (defun +corfu-move-to-minibuffer ()
    "Run the current corfu completion in the minibuffer instead."
    (interactive)
    (let* ((data completion-in-region--data)
           (beg (nth 0 data))
           (end (nth 1 data))
           (table (nth 2 data))
           (pred (nth 3 data))
           (completion-extra-properties (nth 4 data))
           completion-cycle-threshold
           completion-cycling)
      (consult-completion-in-region beg end table pred)))

  ;; tab-and-go keys; RET stays a real newline (like the old company-tng-mode)
  (general-define-key
   :keymaps 'corfu-map
   "TAB" 'corfu-next
   [tab] 'corfu-next
   "S-TAB" 'corfu-previous
   [backtab] 'corfu-previous
   "RET" nil
   [return] nil
   "M-SPC" 'corfu-insert-separator ; type spaces for orderless matching
   "M-m" '+corfu-move-to-minibuffer))

(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)        ; == company-files
  :config
  ;; Make the primary capfs non-exclusive so file/prose capfs can also fire
  ;; instead of an exclusive capf (e.g. lsp) shadowing them.
  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive))
  (with-eval-after-load 'comint
    (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive))
  (with-eval-after-load 'pcomplete
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)))

(use-package nerd-icons-corfu
  :straight t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet-capf
  :straight t
  :after (corfu yasnippet)
  :config
  ;; On-demand snippet completion (inline/merged snippets were too noisy).
  (defun +corfu-yasnippet ()
    "Complete with yasnippet snippets only."
    (interactive)
    (cape-interactive #'yasnippet-capf))
  (general-define-key
   :keymaps 'yas-minor-mode-map
   :states 'insert
   "C-." '+corfu-yasnippet))
