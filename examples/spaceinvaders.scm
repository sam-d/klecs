(import (tui base)
    (tui widgets)
    (klecs ecs)
    (klecs bitset)
    (chezscheme))

(define (current-milliseconds)
(ceiling (let ((t (current-time 'time-monotonic))) (/ (+ (* (time-second t) 1000000000) (time-nanosecond t)) 1000000))))
;TUI elements and variables
(define width 80)
(define height 20)
(define win-screen (color (bg 130) (fg 156) (style slow-blink bold "#     #                  #     # ####### #     #    ### ### ### ### 
 #   #   ####  #    #    #  #  # #     # ##    #    ### ### ### ### 
  # #   #    # #    #    #  #  # #     # # #   #    ### ### ### ### 
   #    #    # #    #    #  #  # #     # #  #  #     #   #   #   #  
   #    #    # #    #    #  #  # #     # #   # #                    
   #    #    # #    #    #  #  # #     # #    ##    ### ### ### ### 
   #     ####   ####      ## ##  ####### #     #    ### ### ### ###")))
(define lose-screen (color (bg 140) (style slow-blink bold "#     #                  #       #######  #####  ####### 
 #   #   ####  #    #    #       #     # #     #    #    
  # #   #    # #    #    #       #     # #          #    
   #    #    # #    #    #       #     #  #####     #    
   #    #    # #    #    #       #     #       #    #    
   #    #    # #    #    #       #     # #     #    #    
   #     ####   ####     ####### #######  #####     #    ")))
;game components
(define draw-player '(#\x259E #\x259A))
(define position-component (component 'position (cons (- height 1) (ceiling (/ width 2)))))
(define drawable-component (component 'sprite draw-player))

;game entities
;an entity is simply a list of components
(define player (list (component 'player) position-component drawable-component (component 'bounding-box (cons 2 2))))
;; (define playing-field (list (make-entity (list (cons 'position (cons 0 0))))))
(define down-step 4)
(define field (list (list (component 'border) (component 'position (cons 0 0)) (component 'bounding-box (cons 1 width)))
		(list (component 'border) (component 'position (cons 0 0)) (component 'bounding-box (cons height 1)))
		(list (component 'border) (component 'position (cons 1 width)) (component 'bounding-box (cons height 1)))
		(list (component 'border) (component 'position (cons height 1)) (component 'bounding-box (cons 1 width)))))
(define initial-aliens (do ((v '() (cons (list
							(component 'position (cons row (+ (* (mod col 5) 5) 2)))
							(component 'direction 'right)
							(component 'bounding-box (cons 2 4))
							(component 'alien)
							(component 'animated (list (current-milliseconds) 500 `(#\space #\x259B #\esc #\[ #\7 #\space #\m #\x259C ,@(cursor-down) ,@(cursor-back 3) #\x2506 #\x2507 #\x2506) (style invert `(#\space #\x259b #\esc #\[ #\7 #\space #\m #\x259c ,@(cursor-down) ,@(cursor-back 3) #\x2506 #\x2507 #\x2506))))
							(component 'spawn (list (current-milliseconds) 5000 -1 (list (component 'direction 'down) (component 'bounding-box (cons 1 1)) (component 'bomb) (component 'sprite '(#\x21a1)))))
							(component 'sprite `(#\space #\x259B #\esc #\[ #\7 #\space #\m #\x259C ,@(cursor-down) ,@(cursor-back 3) #\x2506 #\x2507 #\x2506))) v))
			(col 1 (+ col 1))
			(row 2 (if (>= col 5) down-step 2))
			(cnt 0 (+ cnt 1)))
			((>= cnt 10) v)))
;systems
(define world (apply add-entities (apply add-entities (create-world player (list (component 'position (cons 1 1)) (component 'sprite (light-rounded-box width height)))) initial-aliens) field))

(define (render% world)
  (clear-screen%)
  (let-values (((_ pos sprite) (get-components world (query 'position 'sprite) 'position 'sprite)))
    (for-each (lambda(p s) (draw% (at (car p) (cdr p) s))) pos sprite)))

;l0 <= h1 and l1 <= h0
(define (collide? pos1 bb1 pos2 bb2)
    (and
     (and (<= (car pos1) (+ (car pos2) (car bb2))) (<= (car pos2) (+ (car pos1) (car bb1)))) ;x-axis intersection
     (and (<= (cdr pos1) (+ (cdr pos2) (cdr bb2))) (<= (cdr pos2) (+ (cdr pos1) (cdr bb1)))))) ;y-axis intersection

(define (collision-handler/remove query1 query2)
  (lambda (world)
    (let-components world query1 ((pos 'position)
					(bb 'bounding-box))
		  (let-values (((ids p2 bb2) (get-components world query2 'position 'bounding-box)))
		    (fold-left (lambda (w i p b) (if (collide? pos bb p b) (remove-entities (remove-entities world i) id) w)) world ids p2 bb2)))))
(define (switch-direction direction)
  (case direction
    ((left) 'right)
    ((right) 'left)))
(define (handle-out-of-bounds-aliens world)
  (let-components world (query 'alien) ((p 'position)
					(d 'direction)
					(b 'bounding-box))
		  (set-component world id 'position (cons (+ (car p) (if (or (< (cdr p) 2) (> (+ (cdr p) (cdr b)) width)) 4 0)) (cdr p)))
		  (set-component world id 'direction (if (or (< (cdr p) 2) (> (+ (cdr p) (cdr b)) width)) (switch-direction d) d))))
(define (handle-out-of-bounds world)
  (let-components world (query (or 'bomb 'missile)) ((pos 'position))
		  (cond ((< (car pos) 2) (remove-entities world id))
		       ((> (car pos) height) (remove-entities world id))
		       (else world))))
;;TODO: should this be a syntax-rule?
(define (move direction by)
  (lambda (pair)
    (case direction
      ((east) (cons (car pair) (+ (cdr pair) by)))
      ((west) (cons (car pair) (- (cdr pair) by)))
      ((north) (cons (- (car pair) by) (cdr pair)))
      ((south) (cons (+ (car pair) by) (cdr pair))))))

(define (directed-move world)
  (let-components world (query 'position 'direction) ((pos 'position) (dir 'direction))
		  (update-component world id 'position (case dir
						     ((right east) (move 'east 1))
						     ((left west) (move 'west 1))
						     ((up north) (move 'north 1))
						     ((down south) (move 'south 1))) #f)))
(define (spawn-entities world)
  (let-components world (query 'spawn 'position) ((p 'position)(s 'spawn))
		  (if (> (- (current-milliseconds) (car s)) (cadr s))
			     (set-component (add-entities world (cons (component 'position p) (cadddr s))) id 'spawn (cons (current-milliseconds) (cdr s)))
		      world)))
(define (animate world)
  (let-components world (query 'animated 'sprite) ((a 'animated))
		  (if (> (- (current-milliseconds) (car a)) (cadr a))
		      (let* ((world (set-component world id 'sprite (caddr a))) ;update the sprite 
			     (world (set-component world id 'animated (cons* (current-milliseconds) (cadr a) (append (cdddr a) (list (caddr a)))))))
					world);cycle through the sprite list
		      world)))
(define (handle-input world)
  (if (char-ready? (current-input-port))
	(let ((char (read-char (current-output-port))))
		 (case char
		   ((#\q) (exit-loop 'quit))
		   ((#\space) (let-values (((_ pos) (get-components world (query 'player) 'position)))
				(add-entities world (list (component 'position (car pos)) (component 'direction 'north) (component 'missile) (component 'bounding-box (cons 1 1)) (component 'sprite '(#\x2b06))))))
		   ((#\h) (update-component world (query 'player) 'position (lambda (p) (cons (car p) (- (cdr p) 1))) #f))
		   ((#\l) (update-component world (query 'player) 'position (lambda (p) (cons (car p) (+ (cdr p) 1))) #f))
		   (else world)))
      world))
		   ;; ((#\j) (update-component world (query 'player) 'position (lambda (p) (cons (+ 1 (car p)) (cdr p))) #f))
		   ;; ((#\k) (update-component world (query 'player) 'position (lambda (p) (cons (- (car p) 1) (cdr p))) #f))

(define (check-win-loss world)
  (cond ((= (length (set->list ((query 'alien) world))) 0) (exit-loop 'win))
	((= (length (set->list ((query 'player) world))) 0) (exit-loop 'loss))
	((let-components world (query 'alien) ((pos 'position)) (if (> (car pos) (- height 4)) (exit-loop 'loss) world)))
	(else world)))

;apply all systems in order specified by list systems to the world
(define (get-update-function . systems)
  (lambda (world)
    (let update ((w world)
		 (s systems))
      (if (null? s) w (update ((car s) w) (cdr s))))))

(define exit-loop #f)
  (define-syntax event-loop
    (syntax-rules ()
      ((_ world update-state draw cont-variable)
	   (call/cc (lambda (kont)
		      (set! cont-variable kont) ;set continuation to global variable to be called from anywhere and to stop to event loop: This is done in the absence of parameters in R6RS
		      (let loop ((new-state world))
			(sleep (make-time 'time-duration 100000000 0)) ;sleep 1 second
			;; (sleep (make-time 'time-duration 0 1)) ;sleep 1 second
			(draw new-state)
			(loop (update-state new-state))))))))
			;; ((previous-time (current-milliseconds))
			;; 	 (state world))
			;; (sleep (make-time 'time-duration 100000000 0)) ;sleep 1 second
			;; (let* ((now (current-milliseconds))
			;;        (elapsed (- now previous-time))
			;;        (new-state (update-state state)))
			;;        ;; (input (handle-input))
			;;        ;; (new-state (update state input)))
			;;   (draw new-state)
			;;   (loop (current-milliseconds) buffer new-state))))))))

(enable-alternative-buffer%)
(clear-screen%)
(case (event-loop world (get-update-function handle-input spawn-entities
				       (collision-handler/remove (query 'missile) (query 'alien))
				       (collision-handler/remove (query 'player) (query 'bomb))
				       handle-out-of-bounds
				       handle-out-of-bounds-aliens
				       directed-move
				       animate
				       check-win-loss
				       ) render% exit-loop)
  ((win) (clear-screen%)(draw% (at 1 1 win-screen)))
  ((loss) (clear-screen%)(draw% (at 1 1 lose-screen)))
  ((quit) #f)
  (else (assertion-violation 'exit "unknown game outcome")))
(newline)(display "Press any key to quit...")(read-char (current-input-port))
(disable-alternative-buffer%)
