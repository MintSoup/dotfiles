(defun graph (graph-data)
	(insert " ")
	(backward-char)
	(let ((max-elt (seq-max graph-data)))
		(dolist (n graph-data)
			(save-excursion
				(insert-rectangle
				 (append (make-list
						  (- max-elt n) " ")
						 (make-list n "o"))))
			(forward-char))))

(progn
	(end-of-line)
	(insert "\n\n\n\n")
	(graph (--map
			(round (* it it))
			(number-sequence -5 5 0.1))))
