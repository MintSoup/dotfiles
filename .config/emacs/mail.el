;;; -*- lexical-binding: t -*-

(setq gnus-select-method
	  '(nnimap "gmail"
			   (nnimap-address "imap.gmail.com")
			   (nnimap-server-port 993)
			   (nnimap-stream ssl)
			   (nnimap-authinfo-file "~/.authinfo")))


(setq smtpmail-smtp-server "smtp.gmail.com"
	  smtpmail-smtp-service 25
	  send-mail-function 'smtpmail-send-it
	  user-mail-address "areg"
	  message-directory (expand-file-name "mail" user-emacs-directory)
	  gnus-directory (expand-file-name "news" user-emacs-directory)
	  nnfolder-directory (expand-file-name "mail/archive" user-emacs-directory))

(add-hook 'message-mode-hook 'variable-pitch-mode)
