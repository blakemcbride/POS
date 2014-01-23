
;;  POS Example 01 - Simple class creation and use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Define the class

(define-class <myclass>
  (ivars
   (iv1 0))  

  (imeths

   (get-iv1 (self)
	    iv1)
   (set-iv1 (self val)
	    (set! iv1 val)
	    iv1))


  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mydisplay      ;  simple routine to help with examples
  (lambda (x)
    (display x)
    (newline)))


;;  Use the class


(define ins (make <myclass>))

(mydisplay (ins 'get-iv1))

(ins 'set-iv1 11)

(mydisplay (ins 'get-iv1))


;;  Create another instance

(define ins2 (make <myclass>))

(ins2 'set-iv1 22)

(newline)

(mydisplay (ins 'get-iv1))
(mydisplay (ins2 'get-iv1))
