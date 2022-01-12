;;; -*- lexical-binding: t -*-
(add-hook 'eshell-mode-hook
		  (lambda ()
			  (setq-local tab-width 4
						  company-backends '((company-pcomplete company-files company-elisp)))))

;; (add-hook 'eshell-mode-hook 'company-mode)
;; (general-define-key :states 'insert :keymaps 'eshell-mode-map
;; 					"<tab>" 'company-select-next)

(require 'cl-lib)
(require 'company)
(require 'dash)
(require 'pcomplete)
(require 's)

(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--prefix ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   pcomplete-seen
			   pcomplete-norm-func
			   pcomplete-args
			   pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list))
			(pcomplete-completions)
			(buffer-substring (pcomplete-begin) (point)))))

(defun company-pcomplete--candidates ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   (pcomplete-show-list t)
			   pcomplete-seen pcomplete-norm-func
			   pcomplete-args pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list)
			   (candidates (pcomplete-completions))
			   (prefix (buffer-substring (pcomplete-begin) (point)))
			   ;; Collect all possible completions for the current stub
			   (cnds (all-completions pcomplete-stub candidates))
			   (bnds (completion-boundaries pcomplete-stub candidates nil ""))
			   (skip (- (length pcomplete-stub) (car bnds))))
			;; Replace the stub at the beginning of each candidate by the prefix
			(mapcar (lambda (cand)
						(concat prefix (substring cand skip)))
					cnds))))

;;;###autoload
(defun company-pcomplete-available ()
	(when (eq company-pcomplete-available 'unknown)
		(condition-case _err
				(progn
					(company-pcomplete--candidates)
					(setq company-pcomplete-available t))
			(error
			 (message "Company: pcomplete not found")
			 (setq company-pcomplete-available nil))))
	company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional _arg &rest ignored)
	"`company-mode' completion backend using `pcomplete'."
	(interactive (list 'interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-pcomplete))
		(prefix (when (company-pcomplete-available)
					(company-pcomplete--prefix)))
		(candidates (company-pcomplete--candidates))
		(sorted t)))(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--prefix ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   pcomplete-seen
			   pcomplete-norm-func
			   pcomplete-args
			   pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list))
			(pcomplete-completions)
			(buffer-substring (pcomplete-begin) (point)))))

(defun company-pcomplete--candidates ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   (pcomplete-show-list t)
			   pcomplete-seen pcomplete-norm-func
			   pcomplete-args pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list)
			   (candidates (pcomplete-completions))
			   (prefix (buffer-substring (pcomplete-begin) (point)))
			   ;; Collect all possible completions for the current stub
			   (cnds (all-completions pcomplete-stub candidates))
			   (bnds (completion-boundaries pcomplete-stub candidates nil ""))
			   (skip (- (length pcomplete-stub) (car bnds))))
			;; Replace the stub at the beginning of each candidate by the prefix
			(mapcar (lambda (cand)
						(concat prefix (substring cand skip)))
					cnds))))

;;;###autoload
(defun company-pcomplete-available ()
	(when (eq company-pcomplete-available 'unknown)
		(condition-case _err
				(progn
					(company-pcomplete--candidates)
					(setq company-pcomplete-available t))
			(error
			 (message "Company: pcomplete not found")
			 (setq company-pcomplete-available nil))))
	company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional _arg &rest ignored)
	"`company-mode' completion backend using `pcomplete'."
	(interactive (list 'interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-pcomplete))
		(prefix (when (company-pcomplete-available)
					(company-pcomplete--prefix)))
		(candidates (company-pcomplete--candidates))
		(sorted t)))(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--prefix ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   pcomplete-seen
			   pcomplete-norm-func
			   pcomplete-args
			   pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list))
			(pcomplete-completions)
			(buffer-substring (pcomplete-begin) (point)))))

(defun company-pcomplete--candidates ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   (pcomplete-show-list t)
			   pcomplete-seen pcomplete-norm-func
			   pcomplete-args pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list)
			   (candidates (pcomplete-completions))
			   (prefix (buffer-substring (pcomplete-begin) (point)))
			   ;; Collect all possible completions for the current stub
			   (cnds (all-completions pcomplete-stub candidates))
			   (bnds (completion-boundaries pcomplete-stub candidates nil ""))
			   (skip (- (length pcomplete-stub) (car bnds))))
			;; Replace the stub at the beginning of each candidate by the prefix
			(mapcar (lambda (cand)
						(concat prefix (substring cand skip)))
					cnds))))

;;;###autoload
(defun company-pcomplete-available ()
	(when (eq company-pcomplete-available 'unknown)
		(condition-case _err
				(progn
					(company-pcomplete--candidates)
					(setq company-pcomplete-available t))
			(error
			 (message "Company: pcomplete not found")
			 (setq company-pcomplete-available nil))))
	company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional _arg &rest ignored)
	"`company-mode' completion backend using `pcomplete'."
	(interactive (list 'interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-pcomplete))
		(prefix (when (company-pcomplete-available)
					(company-pcomplete--prefix)))
		(candidates (company-pcomplete--candidates))
		(sorted t)))(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--prefix ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   pcomplete-seen
			   pcomplete-norm-func
			   pcomplete-args
			   pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list))
			(pcomplete-completions)
			(buffer-substring (pcomplete-begin) (point)))))

(defun company-pcomplete--candidates ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   (pcomplete-show-list t)
			   pcomplete-seen pcomplete-norm-func
			   pcomplete-args pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list)
			   (candidates (pcomplete-completions))
			   (prefix (buffer-substring (pcomplete-begin) (point)))
			   ;; Collect all possible completions for the current stub
			   (cnds (all-completions pcomplete-stub candidates))
			   (bnds (completion-boundaries pcomplete-stub candidates nil ""))
			   (skip (- (length pcomplete-stub) (car bnds))))
			;; Replace the stub at the beginning of each candidate by the prefix
			(mapcar (lambda (cand)
						(concat prefix (substring cand skip)))
					cnds))))

;;;###autoload
(defun company-pcomplete-available ()
	(when (eq company-pcomplete-available 'unknown)
		(condition-case _err
				(progn
					(company-pcomplete--candidates)
					(setq company-pcomplete-available t))
			(error
			 (message "Company: pcomplete not found")
			 (setq company-pcomplete-available nil))))
	company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional _arg &rest ignored)
	"`company-mode' completion backend using `pcomplete'."
	(interactive (list 'interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-pcomplete))
		(prefix (when (company-pcomplete-available)
					(company-pcomplete--prefix)))
		(candidates (company-pcomplete--candidates))
		(sorted t)))(defvar company-pcomplete-available 'unknown)

(defun company-pcomplete--prefix ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   pcomplete-seen
			   pcomplete-norm-func
			   pcomplete-args
			   pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list))
			(pcomplete-completions)
			(buffer-substring (pcomplete-begin) (point)))))

(defun company-pcomplete--candidates ()
	(with-no-warnings
		(let* ((pcomplete-stub)
			   (pcomplete-show-list t)
			   pcomplete-seen pcomplete-norm-func
			   pcomplete-args pcomplete-last pcomplete-index
			   (pcomplete-autolist pcomplete-autolist)
			   (pcomplete-suffix-list pcomplete-suffix-list)
			   (candidates (pcomplete-completions))
			   (prefix (buffer-substring (pcomplete-begin) (point)))
			   ;; Collect all possible completions for the current stub
			   (cnds (all-completions pcomplete-stub candidates))
			   (bnds (completion-boundaries pcomplete-stub candidates nil ""))
			   (skip (- (length pcomplete-stub) (car bnds))))
			;; Replace the stub at the beginning of each candidate by the prefix
			(mapcar (lambda (cand)
						(concat prefix (substring cand skip)))
					cnds))))

;;;###autoload
(defun company-pcomplete-available ()
	(when (eq company-pcomplete-available 'unknown)
		(condition-case _err
				(progn
					(company-pcomplete--candidates)
					(setq company-pcomplete-available t))
			(error
			 (message "Company: pcomplete not found")
			 (setq company-pcomplete-available nil))))
	company-pcomplete-available)

;;;###autoload
(defun company-pcomplete (command &optional _arg &rest ignored)
	"`company-mode' completion backend using `pcomplete'."
	(interactive (list 'interactive))
	(cl-case command
		(interactive (company-begin-backend 'company-pcomplete))
		(prefix (when (company-pcomplete-available)
					(company-pcomplete--prefix)))
		(candidates (company-pcomplete--candidates))
		(sorted t)))
