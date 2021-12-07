(defun graph (graph-data)
	(insert " ")
	(backward-char)
	(let ((max-elt (seq-max graph-data)))
		(dolist (n graph-data)
			(save-excursion
				(insert-rectangle
				 (append
				  (make-list (- max-elt n) " ")
				  (make-list n "*"))))
			(forward-char))))

(progn
	(end-of-line)
	(insert "\n\n\n\n")
	(graph (mapcar
		(lambda (x)
			(round (exp x)))
		(number-sequence 0 3 0.1))))
