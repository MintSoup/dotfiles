;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DO NOT PUSH THIS FILE TO GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq ;; user-mail-address	      "mintsoup@airmail.cc"
 smtpmail-smtp-server    "mail.cock.li"
 smtpmail-smtp-service   465
 smtpmail-stream-type    'ssl)

(setq gnus-select-method
	  '(nnimap "cockli"
			   (nnimap-address "mail.cock.li")
			   (nnimap-server-port 993)
			   (nnimap-stream ssl)
			   (nnimap-authinfo-file "~/.authinfo")
			   (nnmail-expiry-wait immediate)))
