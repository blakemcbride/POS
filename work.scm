

;;  The following was the file I was playing with while designing POS
;;  It isn't using any macros so it's easier to see what is going on
;;  This version is incomplete since I countinued my development with
;;  the macro version once I got it working.  So this is just for reference.
;;    --  Blake McBride (blake@mcbride.name)


(define *no-method* '*no-method*)

(define execute-superclasses
  (lambda (scl args)
    (let ((ret '()))
      (let loop ((scl scl))
	(cond ((pair? scl)
	       (set! ret (apply (car scl) args))
	       (if (eq? ret *no-method*)
		   (loop (cdr scl))
		   ret))
	      (else *no-method*))))))

(define make
  (lambda (class . args)
    (let ((ins (apply class (cons 'make args))))
      (ins 'set-class class)
      ins)))

(define make-super-instances
  (lambda (class-list)
    (let ((ret '()))
      (let loop ((sc class-list))
	(cond ((pair? sc)
	       (set! ret (append ret (list (apply (car sc) (list 'make)))))
	       (loop (cdr sc)))
	      (else ret))))))


(define try1
  (let ((super-classes (list))
	(cv1 '()))
    (lambda cargs
      (if (not (pair? cargs))
	  *no-method*
	  (case (car cargs)
	    ((make)
	     (let ((ins
		    (let ((class '())
			  (super-instances (make-super-instances super-classes))
			  (iv1 0))
		      (lambda iargs
			(if (not (pair? iargs))
			    *no-method*
			    (case (car iargs)
			      ((class)
			       class)
			      ((set-class)
			       (set! class (cadr iargs)))
			      ((get-iv1)
			       iv1)
			      ((inc-iv1)
			       (set! iv1 (+ iv1 1))
			       iv1)
			      ((set-iv1)
			       (apply (lambda (val) 
				  (set! iv1 val)
				  iv1)
				(cdr iargs)))
			      (else (execute-superclasses super-instances iargs))))))))
	       ins))
	    ((get-cv1)
	     cv1)
	    ((set-cv1)
	     (set! cv1 (cadr cargs))
	     cv1)
	    (else (execute-superclasses super-classes cargs)))))))
 	     
(define try2
  (let ((super-classes (list))
	(cv2 '()))
    (lambda cargs
      (if (not (pair? cargs))
	  *no-method*
	  (case (car cargs)
	    ((make)
	     (let ((ins
		    (let ((class '())
			  (super-instances (make-super-instances super-classes))
			  (iv2 0))
		      (lambda iargs
			(if (pair? iargs)
			    (case (car iargs)
			      ((class)
			       class)
			      ((set-class)
			       (set! class (cadr iargs)))
			      ((get-iv2)
			       iv2)
			      ((inc-iv2)
			       (set! iv2 (+ iv2 1))
			       iv2)
			      ((set-iv2)
			       (set! iv2 (cadr iargs))
			       iv2)
			      (else (execute-superclasses super-instances iargs))))))))
	       ins))
	    ((get-cv2)
	     cv2)
	    ((set-cv2)
	     (set! cv2 (cadr cargs))
	     cv2)
	    (else (execute-superclasses super-classes cargs)))))))


	     
   
(define try3
  (let ((super-classes (list try1 try2))
	(cv3 '()))
    (lambda cargs
      (if (not (pair? cargs))
	  *no-method*
	  (case (car cargs)
	    ((make)
	     (let ((ins
		    (let ((class '())
			  (super-instances (make-super-instances super-classes))
			  (iv3 0))
		      (lambda iargs
			(if (pair? iargs)
			    (case (car iargs)
			      ((class)
			       class)
			      ((set-class)
			       (set! class (cadr iargs)))
			      ((get-iv3)
			       iv3)
			      ((inc-iv3)
			       (set! iv3 (+ iv3 1))
			       iv3)
			      ((set-iv3)
			       (set! iv3 (cadr iargs))
			       iv3)
			      (else (execute-superclasses super-instances iargs))))))))
	       ins))
	    ((get-cv3)
	     cv3)
	    ((set-cv3)
	     (set! cv3 (cadr cargs))
	     cv3)
	    (else (execute-superclasses super-classes cargs)))))))


	     

