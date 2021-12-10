;;; -*- lexical-binding: t -*-
(add-hook 'eshell-mode-hook
		  (lambda ()
			  (setq tab-width 4
					company-backends '((company-pcomplete)))))

(add-hook 'eshell-mode-hook 'company-mode)
(general-define-key :states 'insert :keymaps 'eshell-mode-map
					"<tab>" 'company-pcomplete)

(defun company-pcomplete--overlap-tail (a b)
	"When A is \"SomeDev\" and B is \"Developer\", return \"eloper\"."
	(let ((prefix a)
          (remaining nil))
		(while (and (not remaining) (> (length prefix) 0))
			(when (s-starts-with? prefix b)
				(setq remaining (substring b (length prefix))))
			(setq prefix (substring prefix 1)))
		remaining))

(defun company-pcomplete--candidates (prefix)
  "Get candidates for PREFIX company completion using `pcomplete'."
  ;; When prefix is: "~/Down" and completion is "Downloads", need
  ;; to find common string and join into "~/Downloads/".
  (-map (lambda (item)
          (if (s-starts-with? prefix item)
              item
            (concat prefix (company-pcomplete--overlap-tail prefix item))))
        (all-completions prefix (pcomplete-completions))))

(defun company-pcomplete (command &optional arg &rest ignored)
  "Complete using pcomplete. See `company''s COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pcomplete))
    (prefix (company-grab-symbol))
    (candidates
     (company-pcomplete--candidates arg))))
