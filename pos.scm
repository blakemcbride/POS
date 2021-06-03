

;;  Portable Object System (POS)
;;  Release 1.0  10/17/06
;;  Written by Blake McBride (blake@mcbridemail.com)
;;
;;  Copyright (c) 2006 Blake McBride
;;  All rights reserved.
;;  
;;  Free for any use so long as this note remains intact.
;;
;;  No warranty of any kind either express or implied.
;;  Use at your own risk.

(define *no-method* '*no-method*)


(define make
  (lambda (class)
    (let ((ins (apply class (list 'make))))
      (ins 'set-class class)
      ins)))


(define-syntax define-class
  (syntax-rules (superclasses cvars ivars cmeths imeths fix-args)
    ((_ class-name (superclasses sc1 ...) (cvars (cv1 icval) ...) (ivars (iv1 iival) ...) (cmeths (cmeth-name carg-list cmeth-defn ...) ...)  (imeths (meth-name arg-list meth-defn ...) ...))
     (begin
       (define class-name
	 (let ((super-classes (list sc1 ...))
	       (cls-name (symbol->string (quote class-name)))
	       (cself '())
	       (cv1 icval) ...)
	   (lambda cargs
	     (define make-super-instances
	       (lambda (class-list)
		 (let ((ret '()))
		   (let loop ((sc class-list))
		     (cond ((pair? sc)
			    (set! ret (append ret (list (apply (car sc) (list 'make)))))
			    (loop (cdr sc)))
			   (else ret))))))
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
	     (if (not (pair? cargs))
		 *no-method*
		 (case (car cargs)
		   ((make)
		    (let ((ins
			   (let ((class '())
				 (self '())
				 (super-instances (make-super-instances super-classes))
				 (iv1 iival) ...)
			     (lambda iargs
			       (if (not (pair? iargs))
				   *no-method*
				   (case (car iargs)
				     ((class)
				      class)
				     ((set-class)
				      (set! class (cadr iargs)))
				     ((set-self)
				      (set! self (cadr iargs)))
				     ((super)
				      (execute-superclasses super-instances (cdr iargs)))
				     ((meth-name)
				      (apply (lambda arg-list
					       meth-defn ...)
					     (cons self (cdr iargs))))
				     ...
				     (else (execute-superclasses super-instances iargs))))))))
		      (ins 'set-self ins)
		      ins))
		   ((set-self)
		    (set! cself (cadr cargs)))
		   ((name)
		    cls-name)
		   ((class)
		    #f)
		   ((super)
		    (execute-superclasses super-classes (cdr cargs)))
		   ((cmeth-name)
		    (apply (lambda carg-list
			     cmeth-defn ...)
			   (cons cself (cdr cargs))))
		   ...
		   (else (execute-superclasses super-classes cargs)))))))
       (class-name 'set-self class-name)))
					;  The following code allows me to take any keyword argument in any order
					;  and allows for missing parameters
    ((_ class-name (fix-args sc cv iv cm im) (superclasses arga ...) argb ...)
     (define-class class-name (fix-args (superclasses arga ...) cv iv cm im) argb ...))
    ((_ class-name (fix-args sc cv iv cm im) (cvars arga ...) argb ...)
     (define-class class-name (fix-args sc (cvars arga ...) iv cm im) argb ...))
    ((_ class-name (fix-args sc cv iv cm im) (ivars arga ...) argb ...)
     (define-class class-name (fix-args sc cv (ivars arga ...) cm im) argb ...))
    ((_ class-name (fix-args sc cv iv cm im) (cmeths arga ...) argb ...)
     (define-class class-name (fix-args sc cv iv (cmeths arga ...) im) argb ...))
    ((_ class-name (fix-args sc cv iv cm im) (imeths arga ...) argb ...)
     (define-class class-name (fix-args sc cv iv cm (imeths arga ...)) argb ...))
    ((_ class-name (fix-args sc cv iv cm im))
     (define-class class-name sc cv iv cm im))
    ((_ class-name (fix-args sc cv iv cm im) arga argb ...)
     (define-class class-name (fix-args sc cv iv cm im) argb ...))
    ((_ class-name arg1 ...)
     (define-class class-name (fix-args (superclasses) (cvars) (ivars) (cmeths) (imeths)) arg1 ...))))




;;  End of file
