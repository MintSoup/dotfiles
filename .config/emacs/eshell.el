;;; -*- lexical-binding: t -*-

(defun eshell-setup-company ()
  (setq-local company-backends
			  '(company-capf)))

(add-hook 'eshell-mode-hook 'company-mode)
(add-hook 'eshell-mode-hook 'eshell-setup-company)
(setq eshell-hist-ignoredups t)

(my-local-leader :keymaps 'eshell-mode-map
  "b" '(eshell-insert-buffer-name :wk "Insert buffer name")
  "d" '(eshell-cd-interactive :wk "Cd"))

(defun eshell-at (ARG)
  (interactive "DOpen Eshell in: ")
  (let* ((default-directory ARG)
		 (eshell-buffer-name
		  (format "*eshell %s*" (file-name-nondirectory
								 (directory-file-name default-directory)))))
	(eshell)))

(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
		 (len (+ (1- (length components))
				 (reduce '+ components :key 'length)))
		 (str ""))
	(while (and (> len max-len)
				(cdr components))
	  (setq str (concat str
						(cond ((= 0 (length (car components))) "/")
							  ((= 1 (length (car components)))
							   (concat (car components) "/"))
							  (t
							   (if (string= "."
											(string (elt (car components) 0)))
								   (concat (substring (car components) 0 2)
										   "/")
								 (string (elt (car components) 0) ?/)))))
			len (- len (1- (length (car components))))
			components (cdr components)))
	(concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(require 'cl)
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun my-eshell-prompt ()
  (concat (with-face "(" :foreground (doom-color 'magenta))
		  (with-face (user-login-name) :foreground (doom-color 'blue))
		  (with-face (system-name) :foreground (doom-color 'red))
		  (with-face ") " :foreground (doom-color 'magenta))
		  (with-face (fish-path (eshell/pwd) 10) :foreground (doom-color 'green))
		  (with-face " $" :foreground (doom-color 'orange))
		  (with-face " " :foreground (doom-color 'violet))))

(setq eshell-prompt-function 'my-eshell-prompt)
(setq eshell-prompt-regexp "([a-z]+) .* \\$ ")

(defun eshell-cd-interactive (dir)
  (interactive "DCd into: ")
  (eshell/cd dir)
  (if (string-empty-p (eshell-get-old-input))
	  (eshell-send-input)
	(save-excursion
	  (beginning-of-line)
	  (let ((prompt-end
			 (save-excursion
			   (eshell-skip-prompt)
			   (point)))
			(inhibit-read-only t))
		(delete-region (point) prompt-end))
	  (eshell-emit-prompt))))
