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


(defun graph-line (graph-data)
	(insert " ")
	(backward-char)
	(let ((max-elt (seq-max graph-data)))
		(dolist (n graph-data)
			(save-excursion
				(insert-rectangle
				 (append
				  (make-list (- max-elt n) " ")
				  (list "*")
				  (make-list (- n 1) " "))))
			(forward-char))))
(progn
	(end-of-line)
	(insert "\n\n\n\n")
	(graph (mapcar
		(lambda (x)
			(round (* x x)))
		(number-sequence -5 5 0.1))))
