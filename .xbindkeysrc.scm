(define actionperformed 0)

(define (first-binding)
  "First binding"
  (xbindkey-function '("b:9") b9-second-binding)
  (xbindkey-function '("b:8") b8-second-binding))


(define (reset-first-binding)
  "reset first binding"
  (ungrab-all-keys)
  (remove-all-keys)
  ;; Set Action Performed state back to 0
  (set! actionperformed 0)
  ;; Forcefully release all modifier keys!
  (run-command "xdotool keyup ctrl keyup alt keyup shift keyup super&")
  (first-binding)
  (grab-all-keys))


(define (b9-second-binding)
  "Front Shoulder Button Extra Functions"
  (ungrab-all-keys)
  (remove-all-keys)
  (run-command "awesome-client 'slowonly = true resync()'")
  (xbindkey-function '("b:4")
					 (lambda ()
					   (run-command "awesome-client 'up()'")
					   (set! actionperformed 1)))

  (xbindkey-function '("b:5")
					 (lambda ()
					   (run-command "awesome-client 'down()'")
					   (set! actionperformed 1)))

  (xbindkey-function '("b:1")
					 (lambda ()
					   (run-command "awesome-client 'left()'")
					   (set! actionperformed 1)))

  (xbindkey-function '("b:3")
					 (lambda ()
					   (run-command "awesome-client 'right()'")
					   (set! actionperformed 1)))

  (xbindkey-function '(release "b:9")
					 (lambda ()
					   ;; Perform Action if Button 8 is pressed and released by itself
					   (run-command "awesome-client 'slowonly = false resync()'")
					   (reset-first-binding)))
  (grab-all-keys))


(define (b8-second-binding)
  "Rear Shoulder Button Extra Functions"
  (ungrab-all-keys)
  (remove-all-keys)

  (run-command "awesome-client 'slowonly = true resync()'")


  ;; Scroll Up
  (xbindkey-function '("b:4")
					 (lambda ()
					   ;; Emulate Alt+Shift+Tab (previous window)
					   (run-command "awesome-client 'mypulse:raise()'")
					   (set! actionperformed 1)))

  ;; Scroll Down
  (xbindkey-function '("b:5")
					 (lambda ()
					   ;; Emulate Alt+Tab (next window)
					   (run-command "awesome-client 'mypulse:lower()'")
					   (set! actionperformed 1)))

  (xbindkey-function '(release "b:8") (lambda ()
										;; Perform Action if Button 8 is pressed and released by itself
										(reset-first-binding)
										(run-command "awesome-client 'slowonly = false resync()'")))
  (grab-all-keys))

;; (debug)
(first-binding)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
										; End of xbindkeys configuration ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
