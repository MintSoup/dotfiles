;;; -*- lexical-binding: t -*-

(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :config
  (general-define-key
   :keymaps 'yas-minor-mode-map
   "TAB" nil
   "<tab>" nil)
  (general-define-key
   :keymaps 'yas-minor-mode-map
   :states 'insert
   "C-e" 'yas-expand)

  (general-define-key
   :keymaps 'yas-keymap
   "C-c" 'yas-next-field
   "C-S-c" 'yas-prev-field
   "<tab>" nil))

(use-package yasnippet-snippets
  :straight t
  :config
  (yas-reload-all))
