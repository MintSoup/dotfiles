;;; -*- lexical-binding: t -*-
(setq
 send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'starttls
 smtpmail-smtp-service 587
 message-directory (expand-file-name "mail" user-emacs-directory)
 gnus-directory (expand-file-name "news" user-emacs-directory)
 nnfolder-directory (expand-file-name "mail/archive" user-emacs-directory))

(add-hook 'message-mode-hook 'variable-pitch-mode)
