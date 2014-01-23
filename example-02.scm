;;  This is a file I used to test POS
;;  It contains use of most of the features of the system
;;    Blake McBride  (blake@mcbride.name)

(define-class <cls1>
  (cvars
   (cv1 '()))                      ; class variables with initial values
  (ivars
   (iv1 4))                        ; instance variables with initial values

                                   ; class methods
  (cmeths
   (get-cv1 (self)
	    cv1)
   (set-cv1 (self val)
	    (set! cv1 val)
	    cv1))

                                   ; instance methods
  (imeths
   (get-iv1 (self)
	    iv1)
   (set-iv1 (self val)
	    (set! iv1 val)
	    iv1)))

(define-class <cls2>
  (cvars
   (cv2 '()))                      ; class variables with initial values
  (ivars
   (iv2 4))                        ; instance variables with initial values

                                   ; class methods
  (cmeths
   (get-cv2 (self)
	    cv2)
   (set-cv2 (self val)
	    (set! cv2 val)
	    cv2))

                                   ; instance methods
  (imeths
   (get-iv2 (self)
	    iv2)

   (fun1 (self)
	 (display "in <cls2>:fun1")
	 (newline))
   
   (set-iv2 (self val)
	    (set! iv2 val)
	    iv2)))

(define-class <cls3>
  (superclasses
   <cls1> <cls2>)                  ; list of super-classes
  (cvars
   (cv3 '()))                      ; class variables with initial values
  (ivars
   (iv3 4))                        ; instance variables with initial values

                                   ; class methods
  (cmeths
   (get-cv3 (self)
	    cv3)
   (set-cv3 (self val)
	    (set! cv3 val)
	    cv3)
   (new (self val)
	((make self) 'init val)))

                                   ; instance methods
  (imeths

   (init (self val)
	 (set! iv3 val)
	 self)
	 
   (get-iv3 (self)
	    iv3)

   (fun1 (self)
	 (self 'super 'fun1)           ;  run the super method
	 (display "in <cls3>:fun1")
	 (newline))

   
   (set-iv3 (self val)
	    (set! iv3 val)
	    iv3)))

(define x (make <cls3>))

(<cls3> 'set-cv1 55)

(<cls1> 'get-cv1)

(x 'set-iv1 14)

(eq? (x 'class) <cls3>)

(define y (<cls3> 'new 88))
(x 'get-iv3)
(y 'get-iv3)
