(defun graph (graph-data)
	(insert " ")
	(backward-char)
	(let ((max-elt (seq-max graph-data)))
		(dolist (n graph-data)
			(save-excursion
				(insert-rectangle
				 (append (make-list
						  (- max-elt n) " ")
						 (make-list n "x"))))
			(forward-char))))

(progn
	(end-of-line)
	(insert "\n\n\n\n")
	(graph (--map
			(round (exp it))
			(number-sequence 0 3 0.1))))



                              x
                              x
                             xx
                             xx
                            xxx
                           xxxx
                           xxxx
                          xxxxx
                         xxxxxx
                        xxxxxxx
                       xxxxxxxx
                      xxxxxxxxx
                     xxxxxxxxxx
                   xxxxxxxxxxxx
                  xxxxxxxxxxxxx
                xxxxxxxxxxxxxxx
             xxxxxxxxxxxxxxxxxx
          xxxxxxxxxxxxxxxxxxxxx
     xxxxxxxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
