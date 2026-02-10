;;; -*- lexical-binding: t -*-
(use-package qml-mode
  :straight t
  :config
  (add-to-list 'lsp-language-id-configuration '(qml-mode . "qml"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("qmlls"))
                    :activation-fn (lsp-activate-on "qml")
                    :server-id 'qmlls))
  (add-hook 'qml-mode-hook (lambda ()
                             (setq-local electric-indent-chars '(?\n ?\( ?\) ?{ ?} ?\[ ?\] ?\; ?,))
                             (lsp-deferred))))
