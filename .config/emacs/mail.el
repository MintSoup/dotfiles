;;; -*- lexical-binding: t -*-

(setq message-directory (expand-file-name "mail" user-emacs-directory)
	  gnus-directory (expand-file-name "news" user-emacs-directory)
	  nnfolder-directory (expand-file-name "mail/archive" user-emacs-directory)
	  gnus-select-method '(nntp "news.gwene.org")
	  gnus-save-newsrc-file nil)

(add-hook 'message-mode-hook 'variable-pitch-mode)

(setq send-mail-function 'smtpmail-send-it
	  message-send-mail-function 'smtpmail-send-it)
