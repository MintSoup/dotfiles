;;; -*- lexical-binding: t -*-
(add-hook 'eshell-mode-hook
		  (lambda ()
			  (setq-local tab-width 4)))

(add-hook 'eshell-mode-hook 'company-mode)
(general-define-key :states 'insert
					:keymaps 'eshell-mode-map
					"<tab>" 'company-select-next)

(my-local-leader :keymaps 'eshell-mode-map
	"b" '(eshell-insert-buffer-name :wk "Insert buffer name"))

(defun eshell-at (ARG)
	(interactive "D")
	(let* ((default-directory ARG)
           (eshell-buffer-name
			(format "*eshell %s" (file-name-nondirectory
								  (directory-file-name default-directory)))))
		(eshell)))

(require 'cl)

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

(defun with-face (str &rest face-plist)
    (propertize str 'face face-plist))

(defun my-eshell-prompt ()
	(concat

	 (with-face "("
				:foreground (doom-color 'magenta))
	 (with-face (user-login-name)
				:foreground (doom-color 'blue))
	 (with-face (system-name)
				:foreground (doom-color 'red))
	 (with-face ") "
				:foreground (doom-color 'magenta))

	 (with-face (fish-path (eshell/pwd) 10)
				:foreground (doom-color 'green))
	 (with-face " $"
				:foreground (doom-color 'orange))
	 (with-face " " :foreground (doom-color 'violet))))



(setq eshell-prompt-function 'my-eshell-prompt)
(setq eshell-prompt-regexp "([a-z]+) .* \\$ ")


;; Stolen from doom
(defun doom-eshell-overrides ()
  (defun eshell-complete-parse-arguments ()
    "Parse the command line arguments for `pcomplete-argument'."
    (catch 'pcompleted
      (unless (and eshell-no-completion-during-jobs
                   (eshell-interactive-process))
        (let ((end (point-marker))
              (begin (save-excursion (eshell-bol) (point)))
              (posns (list t))
              args delim)
          (when (memq this-command '(pcomplete-expand
                                     pcomplete-expand-and-complete))
            (run-hook-with-args 'eshell-expand-input-functions begin end)
            (if (= begin end)
                (end-of-line))
            (setq end (point-marker)))
          (if (setq delim
                    (catch 'eshell-incomplete
                      (ignore
                       (setq args (eshell-parse-arguments begin end)))))
              (cond ((memq (car delim) '(?\{ ?\<))
                     (setq begin (1+ (cadr delim))
                           args (eshell-parse-arguments begin end)))
                    (t (throw 'pcompleted `(nil ,begin ,end)))))
          (when (get-text-property (1- end) 'comment)
            (throw 'pcompleted `(nil ,begin ,end)))
          (let ((pos begin))
            (while (< pos end)
              (if (get-text-property pos 'arg-begin)
                  (nconc posns (list pos)))
              (setq pos (1+ pos))))
          (setq posns (cdr posns))
          (cl-assert (= (length args) (length posns)))
          (let ((a args)
                (i 0)
                l)
            (while a
              (if (and (consp (car a))
                       (eq (caar a) 'eshell-operator))
                  (setq l i))
              (setq a (cdr a) i (1+ i)))
            (and l
                 (setq args (nthcdr (1+ l) args)
                       posns (nthcdr (1+ l) posns))))
          (cl-assert (= (length args) (length posns)))
          (when (and args (eq (char-syntax (char-before end)) ? )
                     (not (eq (char-before (1- end)) ?\\)))
            (nconc args (list ""))
            (nconc posns (list (point))))
          (cons (mapcar
                 (function
                  (lambda (arg)
                    (let ((val
                           (if (listp arg)
                               (let ((result
                                      (eshell-do-eval
                                       (list 'eshell-commands arg) t)))
                                 (cl-assert (eq (car result) 'quote))
                                 (cadr result))
                             arg)))
                      (if (numberp val)
                          (setq val (number-to-string val)))
                      (or val ""))))
                 args)
                posns))))))

(add-hook 'eshell-cmpl-load-hook #'doom-eshell-overrides)

;; eshell-company-patch.el ends here
