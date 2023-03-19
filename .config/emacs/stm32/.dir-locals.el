((nil . ((compile-command . "make -j12")
		 (debug-command . "make -j12")
		 (+debug-function . (lambda ()
							  (project-with-default-dir
							   (call-interactively #'gdb)))))))
