#!r6rs
(library (klecs ecs)
  (export <world>
	  world?
	  <component>
	  component?
	  component
	  create-world
	  add-entities
	  remove-entities
	  query
	  empty-query?
	  get-components
	  get-single-component
	  add-components
	  update-component
	  set-component
	  let-components
	  compose-worlds
	  !)
  (import (rnrs base)
	  (rnrs lists)
	  (rnrs control)
	  (rnrs hashtables)
	  (rnrs records syntactic)
	  (rnrs syntax-case)
	  (klecs bitset)
	  (only (srfi :43) vector-copy))

  (define-record-type (<world> make-world world?)
    (fields (immutable component-map component-map) ;hashtable mapping a component symbol to a set of entity ids that have this component
	    (immutable entity-vector entity-vector) ;a vector containing entities (which are hashtables of component symbol -> component data/values)
	    (immutable free-ids free-ids) ;set of ids that are free within the current entity-vector
	    (immutable entity-set all))) ;a set of all entity ids that currently exist in the world (i.e. all positions of entity vector that have an hashtable entry)

  ;creating a novel type for components
  (define-record-type (<component> component component?)
    (fields (immutable key component-key)
	    (immutable value component-value))
    (protocol (lambda (new) (case-lambda ((k) (if (symbol? k) (new k #t) (assertion-violation 'component "component key must be a symbol." k)))
					 ((k v) (if (symbol? k) (new k v) (assertion-violation 'component "component key must be a symbol." k)))))))
  
  (define (create-world . list-of-entities)
    (let ((ht (make-eq-hashtable))
	  (v (make-vector (length list-of-entities) #f)))
      (do ((i 0 (+ i 1))
	   (ids '() (cons i ids))
	   (entities list-of-entities (cdr entities)))
	  ((null? entities) (make-world ht v (set) (apply set ids)))
	(let ((e (make-eq-hashtable (length (car entities)))))
	  (for-each (lambda (comp)
		      (hashtable-set! e (component-key comp) (component-value comp));fill entity hashtable with all components
		      (hashtable-set! ht (component-key comp) (set-union (set i) (hashtable-ref ht (component-key comp) (set))))) ;and fill map components->entities
		    (car entities)) 
	  (vector-set! v i e)))))

  (define (add-entities world . list-of-entities)
    (if (null? list-of-entities) world
	(let ((ht (hashtable-copy (component-map world) #t))
	      (v (vector-copy (entity-vector world) 0 (max (- (+ (vector-length (entity-vector world)) (length list-of-entities)) (length (set->list (free-ids world)))) (vector-length (entity-vector world))))));grow vector by amount of new entities minus the number of free slots
	  (let lp ((i (vector-length (entity-vector world)))
		   (free (set->list (free-ids world)))
		   (entities list-of-entities)
		   (ids '()))
	    (if (null? entities)
		(make-world ht v (apply set free) (set-union (all world) (apply set ids)))
		(let ((e (make-eq-hashtable (length (car entities)))))
		  (for-each (lambda (comp)
			      (hashtable-set! e (component-key comp) (component-value comp));fill entity hashtable with all components
			      (hashtable-set! ht (component-key comp) (set-union (set (if (null? free) i (car free))) (hashtable-ref ht (component-key comp) (set))))) ;and fill map components->entities
			    (car entities))
		  (vector-set! v (if (null? free) i (car free)) e)
		  (lp (if (null? free) (+ i 1) i) (if (null? free) free (cdr free)) (cdr entities) (cons (if (null? free) i (car free)) ids))))))))

  ;syntax query returns a function that when applied to a world returns the set of ids that match the query (as a bitset)
  (define (get world type) (hashtable-ref (component-map world) type (set)))
  (define-syntax query
    (syntax-rules ()
      ((_) (lambda (world) (all world)))
      ((_ e1 e2 ...) (lambda (world) (query-helper world e1 e2 ...)))))
  (define-syntax query-helper
    (syntax-rules (and or not)
      ((_ world) (all world)) ;is this reachable??
      ((_ world (and e1 e2 ...)) (set-intersection (query-helper world e1) (query-helper world e2 ...)))
      ((_ world (or e1 e2)) (set-union (query-helper world e1) (query-helper world e2)))
      ((_ world (or e1 e2 e3 e4 ...)) (set-union (query-helper world e1) (query-helper world (or e2 e3 e4 ...))))
      ((_ world (not e1)) (set-difference (query-helper world) (get world e1)))
      ((_ world (not e1 e2 e3 ...)) (set-difference (query-helper world) (query-helper world (and e1 e2 e3 ...))))
      ((_ world type) (get world type))
      ((_ world e1 e2 e3 ...) (query-helper world (and e1 e2 e3 ...))))) ;default conjunction is and

  (define (empty-query? world query)
    (equal? (query world) (set)))
  ;query can be either a function as returned by the query syntactic form that will return a bitset when applied to a world or a bitset
  (define (remove-entities world query)
    (let ((ids (if (procedure? query) (query world) (set query)))
	  (ht (hashtable-copy (component-map world) #t))
	  (v (vector-copy (entity-vector world))))
      (for-each (lambda (i)
		  (vector-for-each (lambda (comp) (hashtable-update! ht comp (lambda (x) (set-difference x ids)) (set))) (hashtable-keys (vector-ref v i))) ;remove entity id from the component map for all components it implements
		  (vector-set! v i #f)) ;clear the position in the vector
		(set->list ids))
      (make-world ht v (set-union (free-ids world) ids) (set-difference (all world) ids))))
  
  (define (add-components world query . list-of-components)
    (if (null? list-of-components)  world
	(let ((ids (if (procedure? query) (set->list (query world)) query))
	      (ht (hashtable-copy (component-map world) #t))
	      (v (vector-copy (entity-vector world))))
	  (when (null? ids) (assertion-violation 'add-components "can only add components to existing entities but query returns an empty set"))
	  (for-each (lambda (i)
		      (let ((e (hashtable-copy (vector-ref v i) #t)))
			(for-each (lambda (comp)
				    (hashtable-set! e (component-key comp) (component-value comp));fill entity hashtable with all components
				    (hashtable-set! ht (component-key comp) (set-union (set i) (hashtable-ref ht (component-key comp) (set))))) ;and fill map components->entities
				  list-of-components)
			(vector-set! v i e)))
		    ids)
	  (make-world ht v (free-ids world) (all world)))))

  ;return all the components values as values, but the first value returned is always the id of the entity from which the component is from
  ;query is either a the results of a the query syntactic form or a single integer
  (define (get-components world query . components)
    (let ((ids (if (procedure? query) (set->list (query world)) (list query))))
       (apply values (cons ids (map (lambda(comp) (map (lambda (id) (hashtable-ref (vector-ref (entity-vector world) id) comp #f)) ids)) components)))))
  ;return the value stored in component 'component' for the single entity returned by query (or a single id)
  (define (get-single-component world query component)
    (unless (or (number? query) (= (length (set->list (query world))) 1)) (assertion-violation 'get-single-component "query must return a single entity"))
    (let-values (((ids comp) (get-components world (if (number? query) (list query) query) component)))
      (car comp)))
  
  ;;works for single argument, to set several values use let-components abstraction
  ;;CAVE: this assumes the component already exists.
  (define (set-component world query component value)
    (let ((ids (if (procedure? query) (set->list (query world)) (list query)))
	  (v (vector-copy (entity-vector world))))
      (for-each (lambda (i) (let ((ht (hashtable-copy (vector-ref v i) #t)))
			      (unless (hashtable-contains? ht component) (assertion-violation 'set-component "entity does not already have the component that should be set" (list i component)))
			      (hashtable-set! ht component value)
			      (vector-set! v i ht))) ids)
      (make-world (component-map world) v (free-ids world) (all world))))
  ;update the value of component 'component' for all entities matching query 'query' by calling the procedure with the previous value or default if the entity has no component 'component'
  ;query is either a procedure create by the query syntactic form or an integer
  (define update-component
    (case-lambda ((world query component procedure) (update-component world query component procedure #f))
		 ((world query component procedure default)
		  (unless (symbol? component) (assertion-violation 'update-component "component needs to be a single symbol" component))
		  (let ((ids (if (procedure? query) (set->list (query world)) (list query)))
			(v (vector-copy (entity-vector world))))
		    (for-each (lambda(id)
				(let ((ht (hashtable-copy (vector-ref v id) #t)))
				  (hashtable-update! ht component procedure default)
				  (vector-set! v id ht))) ids)
		    (make-world (component-map world) v (free-ids world) (all world))))))
  ;this syntactic form allows to iterate over the components of all entities that match the query. The components get bound to the variables as specified. Additionally, within
  ;the scope of body the variable world is bound to the last-updated world and the variable id is the entity id that is currently iterated over
  ;it is mandatory, that all expressions e1 e2 ... within the body return a world? instance
  (define-syntax let-components
    (lambda (x)
      (syntax-case x ()
	[(let-components initial-world query ((var component) ...) e1 e2 ...)
	 (with-syntax ((id (datum->syntax #'let-components 'id))
		       (world (datum->syntax #'let-components 'world)))
		      #`(let-values (((ids var ...) (get-components initial-world query component ...)))
			  (fold-left (lambda (w id var ...)
				       (let* ((world w)
					      #,@(map (lambda (s) #`(world #,s)) #'(e1 e2 ...))) world)) initial-world ids var ...)))])))
  ;chain expressions by substituting the first argument with the result from the last expression, which must return a <world> type
  (define-syntax compose-worlds
    (syntax-rules ()
      ((_ e) e)
      ((_ e1 e2 e3 ...) (compose-worlds (replace-hole e1 e2) e3 ...))))
  (define-syntax replace-hole
      (syntax-rules (!)
	((_ first-expression (p ! e2 ...)) (p first-expression e2 ...))))
  (define-syntax !
    (lambda (x)
      (syntax-violation 'exclamation-point "misplaced auxiliary keyword" x)))
);end of library

