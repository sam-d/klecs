#!r6rs
(library (klecs bitset)
  (export set
	  set-contains?
	  set-intersection
	  set-union
	  set-difference
	  set->list)
  (import (rnrs base)
	  (rnrs lists)
	  (rnrs control)
	  (rnrs arithmetic fixnums)
	  (only (srfi :43) vector-copy))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; implementation of sets of integers backed 
;; by bitsets (represented as fixnums)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a single fixnum can store as many positions as (fxlength (greatest-fixnum)) ;60 on my platform
;; so a bitset if a vector/list of fixnums storing w integers in each entry
;TODO: use vector-set-fixnum! from (chezscheme) instead of vector-set! for optimization
  (define (set . integers)
    (if (null? integers) (vector 0)
	(let* ((word-size (fxlength (greatest-fixnum)))
	       (m (apply max integers))
	       (max-index (+ 1 (div m word-size)))
	       (set (make-vector max-index 0)))
	  (do ((in integers (cdr in)))
	      ((null? in) set)
	    (vector-set! set  (div (car in) word-size) (fxior (vector-ref set (div (car in) word-size)) (fxarithmetic-shift-left 1 (mod (car in) word-size))))))))
  
  (define (set-contains? set val)
    (let* ((word-size (fxlength (greatest-fixnum)))
	   (index (div val word-size)))
      (if (> index (vector-length set)) #f (fxbit-set? (vector-ref set index) (mod val word-size)))))
  
  ;; (define (set-intersection-binary set1 set2)
  ;;   (let ((m (min (vector-length set1) (vector-length set2))))
  ;;     (do ((res (make-vector m))
  ;; 	 (i 0 (+ i 1)))
  ;; 	((>= i m) res)
  ;;       (vector-set! res i (fxand (vector-ref set1 i) (vector-ref set2 i))))))
  
  (define (set-intersection set1 set2 . rest)
    (let* ((all (cons* set1 set2 rest))
	   (m (apply min (map vector-length all))))
      (do ((res (make-vector m))
	   (i 0 (+ i 1)))
	  ((>= i m) res)
	(vector-set! res i (apply fxand (map (lambda(s) (vector-ref s i)) all))))))
  
  ;; (define (set-union set1 set2)
  ;;   (let ((m (min (vector-length set1) (vector-length set2))))
  ;;     (do ((res (if (= (vector-length set1) m) (vector-copy set2) (vector-copy set1))) ;start with largest set
  ;; 	 (i 0 (+ i 1)))
  ;; 	((>= i m) res)
  ;;       (vector-set! res i (fxior (vector-ref set1 i) (vector-ref set2 i))))))
  
  (define (set-union set1 set2 . rest)
    (let* ((all (cons* set1 set2 rest))
	   (lengths (map vector-length all))
	   (m (apply min lengths)))
      (do ((res (vector-copy (do ((vals all (cdr vals))
				  (l lengths (cdr l))
				  (mx (apply max lengths)))
				 ((= mx (car l)) (car vals))))) ;copy largest vector
	   (i 0 (+ i 1)))
	  ((>= i m) res)
	(vector-set! res i (apply fxior (map (lambda(s) (vector-ref s i)) all))))))
  
					;TODO: develop polyadic variant of this operation (maybe as a fold?)
  (define (set-difference set1 set2)
    (let ((m (min (vector-length set1) (vector-length set2))))
      (do ((res (vector-copy set1)) ;start with set1
	   (i 0 (+ i 1)))
	  ((>= i m) res)
	(vector-set! res i (fxif (fxand (vector-ref set1 i) (vector-ref set2 i)) 0 (vector-ref set1 i))))))
  
  (define (set->list set)
    (let ((l (vector-length set))
	  (word-size (fxlength (greatest-fixnum))))
      (let lp1 ((i 0)
		(res '()))
	(if (= i l) res
	    (let lp2 ((bs (vector-ref set i))
		      (j 0)
		      (res2 res))
	      (cond ((= (fxlength bs) j) (lp1 (+ i 1) res2))
		    ((fxbit-set? bs j) (lp2 bs (+ j 1) (cons (+ (* i word-size) j) res2)))
		    (else (lp2 bs (+ j 1) res2))))))))

) ;end of library form
