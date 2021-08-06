;;; -*- lexical-binding: t -*-
(use-package org-superstar
	:straight t
	:hook (org-mode . org-superstar-mode)
	:hook (org-mode . variable-pitch-mode)
	:hook (org-mode . visual-fill-column-mode)
	:hook (org-mode . +org-enable-auto-reformat-tables-h)
	:init
	(setq org-superstar-headline-bullets-list '(9673 9675 10047 10040)
		  org-superstar-leading-bullet ""))

(use-package evil-org
	:straight t
	:init
	(setq evil-org-retain-visual-state-on-shift t)
	:config
	(add-to-list 'evil-org-key-theme 'shift)
	(evil-org-set-key-theme)
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys)
	:hook (org-mode . evil-org-mode))

(use-package org-tempo
	:ensure nil)

;;; Helpers

(defun +org--insert-item (direction)
	(let ((context (org-element-lineage
					(org-element-context)
					'(table table-row headline inlinetask item plain-list)
					t)))
		(pcase (org-element-type context)
			;; Add a new list item (carrying over checkboxes if necessary)
			((or `item `plain-list)
			 ;; Position determines where org-insert-todo-heading and org-insert-item
			 ;; insert the new list item.
			 (if (eq direction 'above)
					 (org-beginning-of-item)
				 (org-end-of-item)
				 (backward-char))
			 (org-insert-item (org-element-property :checkbox context))
			 ;; Handle edge case where current item is empty and bottom of list is
			 ;; flush against a new heading.
			 (when (and (eq direction 'below)
						(eq (org-element-property :contents-begin context)
							(org-element-property :contents-end context)))
				 (org-end-of-item)
				 (org-end-of-line)))

			;; Add a new table row
			((or `table `table-row)
			 (pcase direction
				 ('below (save-excursion (org-table-insert-row t))
						 (org-table-next-row))
				 ('above (save-excursion (org-shiftmetadown))
						 (+org/table-previous-row))))

			;; Otherwise, add a new heading, carrying over any todo state, if
			;; necessary.
			(_
			 (let ((level (or (org-current-level) 1)))
				 ;; I intentionally avoid `org-insert-heading' and the like because they
				 ;; impose unpredictable whitespace rules depending on the cursor
				 ;; position. It's simpler to express this command's responsibility at a
				 ;; lower level than work around all the quirks in org's API.
				 (pcase direction
					 (`below
					  (let (org-insert-heading-respect-content)
						  (goto-char (line-end-position))
						  (org-end-of-subtree)
						  (insert "\n" (make-string level ?*) " ")))
					 (`above
					  (org-back-to-heading)
					  (insert (make-string level ?*) " ")
					  (save-excursion (insert "\n"))))
				 (when-let* ((todo-keyword (org-element-property :todo-keyword context))
							 (todo-type    (org-element-property :todo-type context)))
					 (org-todo
					  (cond ((eq todo-type 'done)
							 ;; Doesn't make sense to create more "DONE" headings
							 (car (+org-get-todo-keywords-for todo-keyword)))
							(todo-keyword)
							('todo)))))))

		(when (org-invisible-p)
			(org-show-hidden-entry))
		(when (and (bound-and-true-p evil-local-mode)
				   (not (evil-emacs-state-p)))
			(evil-insert 1))))

(defun +org--get-property (name &optional bound)
	(save-excursion
		(let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
			(goto-char (point-min))
			(when (re-search-forward re bound t)
				(buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun +org-get-global-property (name &optional file bound)
	"Get a document property named NAME (string) from an org FILE (defaults to
current file). Only scans first 2048 bytes of the document."
	(unless bound
		(setq bound 256))
	(if file
			(with-temp-buffer
				(insert-file-contents-literally file nil 0 bound)
				(+org--get-property name))
		(+org--get-property name bound)))

(defun +org-get-todo-keywords-for (&optional keyword)
	"Returns the list of todo keywords that KEYWORD belongs to."
	(when keyword
		(cl-loop for (type . keyword-spec)
				 in (cl-remove-if-not #'listp org-todo-keywords)
				 for keywords =
				 (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
											 (match-string 1 x)
										 x))
						 keyword-spec)
				 if (eq type 'sequence)
				 if (member keyword keywords)
				 return keywords)))


;;; Commands

(defun +org/dwim-at-point (&optional arg)
	"Do-what-I-mean at point.
	If on a:
	- checkbox list item or todo heading: toggle it.
	- clock: update its time.
	- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
	subtree; update statistics cookies/checkboxes and ToCs.
	- footnote reference: jump to the footnote's definition
	- footnote definition: jump to the first reference of this footnote
	- table-row or a TBLFM: recalculate the table's formulas
	- table-cell: clear it and go into insert mode. If this is a formula cell,
	recaluclate it instead.
	- babel-call: execute the source block
	- statistics-cookie: update it.
	- latex fragment: toggle it.
	- link: follow it
	- otherwise, refresh all inline images in current tree."
	(interactive "P")
	(if (button-at (point))
			(call-interactively #'push-button)
		(let* ((context (org-element-context))
			   (type (org-element-type context)))
			;; skip over unimportant contexts
			(while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
				(setq context (org-element-property :parent context)
					  type (org-element-type context)))
			(pcase type
				(`headline
				 (cond ((memq (bound-and-true-p org-goto-map)
							  (current-active-maps))
						(org-goto-ret))
					   ((and (fboundp 'toc-org-insert-toc)
							 (member "TOC" (org-get-tags)))
						(toc-org-insert-toc)
						(message "Updating table of contents"))
					   ((string= "ARCHIVE" (car-safe (org-get-tags)))
						(org-force-cycle-archived))
					   ((or (org-element-property :todo-type context)
							(org-element-property :scheduled context))
						(org-todo
						 (if (eq (org-element-property :todo-type context) 'done)
								 (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
									 'todo)
							 'done))))
				 ;; Update any metadata or inline previews in this subtree
				 (org-update-checkbox-count)
				 (org-update-parent-todo-statistics)
				 (when (and (fboundp 'toc-org-insert-toc)
							(member "TOC" (org-get-tags)))
					 (toc-org-insert-toc)
					 (message "Updating table of contents"))
				 (let* ((beg (if (org-before-first-heading-p)
									 (line-beginning-position)
								 (save-excursion (org-back-to-heading) (point))))
						(end (if (org-before-first-heading-p)
									 (line-end-position)
								 (save-excursion (org-end-of-subtree) (point))))
						(overlays (ignore-errors (overlays-in beg end)))
						(latex-overlays
						 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
									 overlays))
						(image-overlays

						 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
									 overlays)))
					 (+org--toggle-inline-images-in-subtree beg end)
					 (if (or image-overlays latex-overlays)
							 (org-clear-latex-preview beg end)
						 (org--latex-preview-region beg end))))

				(`clock (org-clock-update-time-maybe))

				(`footnote-reference
				 (org-footnote-goto-definition (org-element-property :label context)))

				(`footnote-definition
				 (org-footnote-goto-previous-reference (org-element-property :label context)))

				((or `planning `timestamp)
				 (org-follow-timestamp-link))

				((or `table `table-row)
				 (if (org-at-TBLFM-p)
						 (org-table-calc-current-TBLFM)
					 (ignore-errors
						 (save-excursion
							 (goto-char (org-element-property :contents-begin context))
							 (org-call-with-arg 'org-table-recalculate (or arg t))))))

				(`table-cell
				 (org-table-blank-field)
				 (org-table-recalculate arg)
				 (when (and (string-empty-p (string-trim (org-table-get-field)))
							(bound-and-true-p evil-local-mode))
					 (evil-change-state 'insert)))

				(`babel-call
				 (org-babel-lob-execute-maybe))

				(`statistics-cookie
				 (save-excursion (org-update-statistics-cookies arg)))

				((or `src-block `inline-src-block)
				 (org-babel-execute-src-block arg))

				((or `latex-fragment `latex-environment)
				 (org-latex-preview arg))

				(`link
				 (let* ((lineage (org-element-lineage context '(link) t))
						(path (org-element-property :path lineage)))
					 (if (or (equal (org-element-property :type lineage) "img")
							 (and path (image-type-from-file-name path)))
							 (+org--toggle-inline-images-in-subtree
							  (org-element-property :begin lineage)
							  (org-element-property :end lineage))
						 (org-open-at-point arg))))

				((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
				 (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
					 (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

				(_
				 (if (or (org-in-regexp org-ts-regexp-both nil t)
						 (org-in-regexp org-tsr-regexp-both nil  t)
						 (org-in-regexp org-link-any-re nil t))
						 (call-interactively #'org-open-at-point)
					 (+org--toggle-inline-images-in-subtree
					  (org-element-property :begin context)
					  (org-element-property :end context))))))))

(defun +org/shift-return (&optional arg)
	"Insert a literal newline, or dwim in tables.
Executes `org-table-copy-down' if in table."
	(interactive "p")
	(if (org-at-table-p)
			(org-table-copy-down arg)
		(org-return nil arg)))


(defun +org/insert-item-below (count)
	"Inserts a new heading, table cell or item below the current one."
	(interactive "p")
	(dotimes (_ count) (+org--insert-item 'below)))

(defun +org/insert-item-above (count)
	"Inserts a new heading, table cell or item above the current one."
	(interactive "p")
	(dotimes (_ count) (+org--insert-item 'above)))


;;; Folds
(defalias #'+org/toggle-fold #'+org-cycle-only-current-subtree-h)

(defun +org/open-fold ()
	"Open the current fold (not but its children)."
	(interactive)
	(+org/toggle-fold t))

(defalias #'+org/close-fold #'outline-hide-subtree)

(defun +org/close-all-folds (&optional level)
	"Close all folds in the buffer (or below LEVEL)."
	(interactive "p")
	(outline-hide-sublevels (or level 1)))

(defun +org/open-all-folds (&optional level)
	"Open all folds in the buffer (or up to LEVEL)."
	(interactive "P")
	(if (integerp level)
			(outline-hide-sublevels level)
		(outline-show-all)))

(defun +org--get-foldlevel ()
	(let ((max 1))
		(save-restriction
			(narrow-to-region (window-start) (window-end))
			(save-excursion
				(goto-char (point-min))
				(while (not (eobp))
					(org-next-visible-heading 1)
					(when (outline-invisible-p (line-end-position))
						(let ((level (org-outline-level)))
							(when (> level max)
								(setq max level))))))
			max)))

(defun +org/show-next-fold-level (&optional count)
	"Decrease the fold-level of the visible area of the buffer. This unfolds
another level of headings on each invocation."
	(interactive "p")
	(let ((new-level (+ (+org--get-foldlevel) (or count 1))))
		(outline-hide-sublevels new-level)
		(message "Folded to level %s" new-level)))

(defun +org/hide-next-fold-level (&optional count)
	"Increase the global fold-level of the visible area of the buffer. This folds
another level of headings on each invocation."
	(interactive "p")
	(let ((new-level (max 1 (- (+org--get-foldlevel) (or count 1)))))
		(outline-hide-sublevels new-level)
		(message "Folded to level %s" new-level)))



(defun +org-cycle-only-current-subtree-h (&optional arg)
	"Toggle the local fold at the point, and no deeper.
`org-cycle's standard behavior is to cycle between three levels: collapsed,
subtree and whole document. This is slow, especially in larger org buffer. Most
of the time I just want to peek into the current subtree -- at most, expand
*only* the current subtree.
All my (performant) foldings needs are met between this and `org-show-subtree'
(on zO for evil users), and `org-cycle' on shift-TAB if I need it."
	(interactive "P")
	(unless (eq this-command 'org-shifttab)
		(save-excursion
			(org-beginning-of-line)
			(let (invisible-p)
				(when (and (org-at-heading-p)
						   (or org-cycle-open-archived-trees
							   (not (member org-archive-tag (org-get-tags))))
						   (or (not arg)
							   (setq invisible-p (outline-invisible-p (line-end-position)))))
					(unless invisible-p
						(setq org-cycle-subtree-status 'subtree))
					(org-cycle-internal-local)
					t)))))

(defun +org-make-last-point-visible-h ()
	"Unfold subtree around point if saveplace places us in a folded region."
	(and (not org-inhibit-startup)
		 (not org-inhibit-startup-visibility-stuff)
		 ;; Must be done on a timer because `org-show-set-visibility' (used by
		 ;; `org-reveal') relies on overlays that aren't immediately available
		 ;; when `org-mode' first initializes.
		 (run-at-time 0.1 nil #'org-reveal '(4))))

;;; lang/org/autoload/org-tables.el -*- lexical-binding: t; -*-

;;
;;; Row/Column traversal

;;;###autoload
(defun +org/table-previous-row ()
	"Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
	(interactive)
	(org-table-maybe-eval-formula)
	(org-table-maybe-recalculate-line)
	(if (and org-table-automatic-realign
			 org-table-may-need-update)
			(org-table-align))
	(let ((col (org-table-current-column)))
		(beginning-of-line 0)
		(when (or (not (org-at-table-p)) (org-at-table-hline-p))
			(beginning-of-line))
		(org-table-goto-column col)
		(skip-chars-backward "^|\n\r")
		(when (org-looking-at-p " ")
			(forward-char))))


;;
;;; Row/Column insertion

;;;###autoload
(defun +org/table-insert-column-left ()
	"Insert a new column left of the current column."
	(interactive)
	(org-table-insert-column)
	(org-table-move-column-left))

;;;###autoload
(defun +org/table-insert-row-below ()
	"Insert a new row below the current row."
	(interactive)
	(org-table-insert-row 'below))

;;
;;; Hooks

;;;###autoload
(defun +org-realign-table-maybe-h ()
	"Auto-align table under cursor."
	(when (and (org-at-table-p) org-table-may-need-update)
		(let ((pt (point))
			  (inhibit-message t))
			(if org-table-may-need-update (org-table-align))
			(goto-char pt))))

;;;###autoload
(defun +org-enable-auto-reformat-tables-h ()
	"Realign tables & update formulas when exiting insert mode (`evil-mode').
Meant for `org-mode-hook'."
	(when (featurep 'evil)
		(add-hook 'evil-insert-state-exit-hook #'+org-realign-table-maybe-h nil t)
		(add-hook 'evil-replace-state-exit-hook #'+org-realign-table-maybe-h nil t)
		(advice-add #'evil-replace :after #'+org-realign-table-maybe-a)))

;;;###autoload
(defun +org-delete-backward-char-and-realign-table-maybe-h ()
	"Ensure deleting characters with backspace doesn't deform the table cell."
	(when (eq major-mode 'org-mode)
		(org-check-before-invisible-edit 'delete-backward)
		(save-match-data
			(when (and (org-at-table-p)
					   (not (org-region-active-p))
					   (string-match-p "|" (buffer-substring (point-at-bol) (point)))
					   (looking-at-p ".*?|"))
				(let ((pos (point))
					  (noalign (looking-at-p "[^|\n\r]*  |"))
					  (c org-table-may-need-update))
					(delete-char -1)
					(unless overwrite-mode
						(skip-chars-forward "^|")
						(insert " ")
						(goto-char (1- pos)))
					;; noalign: if there were two spaces at the end, this field
					;; does not determine the width of the column.
					(when noalign (setq org-table-may-need-update c)))
				t))))


;;; Advice

(general-define-key :keymaps 'org-mode-map :states 'normal
					"S-<return>"    #'+org/shift-return
					"C-<return>"    #'+org/insert-item-below
					"C-S-<return>"  #'+org/insert-item-above
					"C-M-<return>"  #'org-insert-subheading
					"<return>"      #'+org/dwim-at-point
					"S-<return>"    #'+org/shift-return
					"S-h"           #'org-shiftleft
					"S-l"           #'org-shiftright
					"S-k"           #'org-shiftup
					"S-j"           #'org-shiftdown)

(general-define-key :keymaps 'org-mode-map :states 'insert
					"S-<return>"    #'+org/shift-return
					"C-<return>"    #'+org/insert-item-below
					"C-S-<return>"  #'+org/insert-item-above
					"C-M-<return>"  #'org-insert-subheading
					"S-<return>"    #'+org/shift-return
					"M-l"           #'org-metaright
					"M-h"           #'org-metaleft
					"M-j"           #'org-metadown
					"M-k"           #'org-metaleft)

;; Ugly hack to work around evil-org's stupid design
(add-hook 'evil-org-mode-hook
		  (lambda ()
			  (evil-define-minor-mode-key '(normal visual) 'evil-org-mode
				  (kbd "x")'evil-org-delete-char-without-yank
				  (kbd "<C-return>") '+org/insert-item-below
				  (kbd "<C-S-return>") '+org/insert-item-above)))

(add-hook 'org-tab-first-hook '+org-cycle-only-current-subtree-h)

(my-local-leader :keymaps 'org-mode-map
    "#" #'org-update-statistics-cookies
    "'" #'org-edit-special
    "*" #'org-ctrl-c-star
    "+" #'org-ctrl-c-minus
    "," #'org-switchb
    "." #'org-goto
	"." #'counsel-org-goto
	"/" #'counsel-org-goto-all
	"A" #'org-archive-subtree
	"e" #'org-export-dispatch
	"f" #'org-footnote-new
	"h" #'org-toggle-heading
	"i" #'org-toggle-item
	"I" #'org-toggle-inline-images
	"n" #'org-store-link
	"o" #'org-set-property
	"q" #'org-set-tags-command
	"t" #'org-todo
	"T" #'org-todo-list
	"x" #'org-toggle-checkbox

	"a" '(:ignore t :wk "Attach")
	"a a" #'org-attach
	"a d" #'org-attach-delete-one
	"a D" #'org-attach-delete-all
	"a f" #'+org/find-file-in-attachments
	"a l" #'+org/attach-file-and-insert-link
	"a n" #'org-attach-new
	"a o" #'org-attach-open
	"a O" #'org-attach-open-in-emacs
	"a r" #'org-attach-reveal
	"a R" #'org-attach-reveal-in-emacs
	"a u" #'org-attach-url
	"a s" #'org-attach-set-directory
	"a S" #'org-attach-sync

	"b" '(:ignore t :wk "Table")
	"b -" #'org-table-insert-hline
	"b a" #'org-table-align
	"b b" #'org-table-blank-field
	"b c" #'org-table-create-or-convert-from-region
	"b e" #'org-table-edit-field
	"b f" #'org-table-edit-formulas
	"b h" #'org-table-field-info
	"b s" #'org-table-sort-lines
	"b r" #'org-table-recalculate
	"b R" #'org-table-recalculate-buffer-tables

	"b d" '(:ignore t :wk "Delete")
	"b d c" #'org-table-delete-column
	"b d r" #'org-table-kill-row

	"b i" '(:ignore t :wk "Insert")
	"b i c" #'org-table-insert-column
	"b i h" #'org-table-insert-hline
	"b i r" #'org-table-insert-row
	"b i H" #'org-table-hline-and-move

	"b t" '(:ignore t :wk "Toggle")
	"b t f" #'org-table-toggle-formula-debugger
	"b t o" #'org-table-toggle-coordinate-overlays

	"c" '(:ignore t :wk "Clock")
	"c c" #'org-clock-cancel
	"c d" #'org-clock-mark-default-task
	"c e" #'org-clock-modify-effort-estimate
	"c E" #'org-set-effort
	"c g" #'org-clock-goto
	"c l" #'+org/toggle-last-clock
	"c i" #'org-clock-in
	"c I" #'org-clock-in-last
	"c o" #'org-clock-out
	"c r" #'org-resolve-clocks
	"c R" #'org-clock-report
	"c t" #'org-evaluate-time-range
	"c =" #'org-clock-timestamps-up
	"c -" #'org-clock-timestamps-down

	"d" '(:ignore t :wk "Date")
	"d d" #'org-deadline
	"d s" #'org-schedule
	"d t" #'org-time-stamp
	"d T" #'org-time-stamp-inactive

	"g" '(:ignore t :wk "Go to")
	"g g" #'org-goto
	"g g" #'counsel-org-goto
	"g G" #'counsel-org-goto-all
	"g c" #'org-clock-goto
	"g i" #'org-id-goto
	"g r" #'org-refile-goto-last-stored
	"g v" #'+org/goto-visible
	"g x" #'org-capture-goto-last-stored

	"l" '(:ignore t :wk "Link")
	"l c" #'org-cliplink
	"l d" #'+org/remove-link
	"l i" #'org-id-store-link
	"l l" #'org-insert-link
	"l L" #'org-insert-all-links
	"l s" #'org-store-link
	"l S" #'org-insert-last-stored-link
	"l t" #'org-toggle-link-display

	"p" '(:ignore t :wk "Publish")
	"p a" #'org-publish-all
	"p f" #'org-publish-current-file
	"p p" #'org-publish
	"p P" #'org-publish-current-project
	"p s" #'org-publish-sitemap
	"p r" #'org-refile ; to all `org-refile-targets'

	"s" '(:ignore t :wk "Subtree")
	"s a" #'org-toggle-archive-tag
	"s b" #'org-tree-to-indirect-buffer
	"s d" #'org-cut-subtree
	"s h" #'org-promote-subtree
	"s j" #'org-move-subtree-down
	"s k" #'org-move-subtree-up
	"s l" #'org-demote-subtree
	"s n" #'org-narrow-to-subtree
	"s r" #'org-refile
	"s s" #'org-sparse-tree
	"s A" #'org-archive-subtree
	"s N" #'widen
	"s S" #'org-sort

	"p" '(:ignore t :wk "Priority")
	"p d" #'org-priority-down
	"p p" #'org-priority
	"p u" #'org-priority-up)

(setq org-directory "~/Org/"
	  org-agenda-files '("~/Org/Agenda/")
	  org-hide-emphasis-markers t
	  org-indent-indentation-per-level 2
	  org-list-indent-offset 4
	  org-ellipsis " ▾"
	  org-list-allow-alphabetical t
	  org-startup-indented t

	  org-todo-keywords
      '((sequence "TODO(t)" "INPROG(p)" "|" "DONE(d)")
		(sequence "UNREVIEWED(u)" "REVIEW(r)" "INPROG(p)" "|" "DONE(d)" "REJECTED(x)"))

	  org-todo-keyword-faces
	  '(("PROG" . "dark orange")
		("UNREVIEWD" . "#98be65")
		("REVIEW" . "dark turquoise")
		("INPROG" . "dark orange")
		("REJECTED" . "tomato")))

(with-eval-after-load 'org-faces
	(set-face-attribute 'org-level-1 nil :height 1.35 :weight 'normal)
	(set-face-attribute 'org-level-2 nil :height 1.2 :weight 'normal)
	(set-face-attribute 'org-level-3 nil :height 1.05 :weight 'normal)
	(set-face-attribute 'org-level-4 nil :weight 'normal)
	(set-face-attribute 'org-checkbox nil :inherit '(org-todo fixed-pitch))
	(set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch :foreground "#83898d")
	(set-face-attribute 'org-document-title nil :height 1.5 :weight 'normal)
	(set-face-attribute 'org-block nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(with-eval-after-load 'org-indent
	(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))

(with-eval-after-load 'org-superstar
	(set-face-attribute 'org-superstar-header-bullet nil :font "FreeSans" :height 1.0))

(setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
