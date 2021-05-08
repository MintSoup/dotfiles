(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)) ;; one line at a time
	  mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
	  mouse-wheel-follow-mouse 't ;; scroll window under mouse
	  scroll-step 1 ;; keyboard scroll one line at a time
	  scroll-conservatively 10000
	  make-backup-files nil)