;; this is the main function of the lab
; this function calls another function to make sure that xlow and xhigh are set just once
; in recursion another function is called several making it a tail recursion
(defun fresh (a b c d e f)
	(print "My name is Chudamani Aryal. ~%")
	(print "MY ID Number is 1000692493. ~%")
	(print "This is an awesome project. ~%")
	(print "This world is about to end. ~%")
	
	;; values of xlow and xhigh are set
	(float(setq x2 10))
	(float(setq x1 -10))
	
	;; another function is called to make sure that values are set just once
	( fresh1 a b c d e f x1 x2)
)

;; this is a function which is called from the main function.
(defun fresh1 (a b c d e f x1 x2)

	;; ylow and yhigh are calculated so that we can pass them in another function
	(setf y1 (PolyFun a b c d e f x1))
	(setf y2 (PolyFun a b c d e f x2))
	
	;;; this is the function where multiple recursion occurs
	(regula_falsi a b c d e f x1 x2 y1 y2)
)

;; this function is executed mutliple times because we call this function while recursion is needed
(defun regula_falsi (a b c d e f x1 x2 y1 y2)
	
	;; this if statement makes sure that xlow doesn't exceed xhigh
	(if (= x1 x2)
		(progn
			(print "No solution found")
			(setf flag 0)
			(go end)
		)
		()
	)
	(setf tolerance 0.01)		; tolerance is set
	
	; tagbody is need for the go to structure when we find the solution, we need to jump out of the function without executing the rest of the body of the function
	(tagbody
		(cond
			(
				;;;;; first condition compares if product of ylow and yhigh is zero. if yes, then either xlow or xhigh is one of the solutions
				;;;;; if solution found then set the flag in such a way that we never have to recurse a function and go to end
				(= (* y1 y2) 0) 
				(progn
					(if (= y1 0)
						(format t " ~% One of the possible solution is ~10F ~%" x1)
						(format t " ~% One of the possible solution is ~10F ~%" x2)
					)
					(setf flag 0)
					(go end)
				)
			)			
			;;;;;;;;;;;;;;;;;;;;
			; (
				; (< (abs y1) tolerance) 
				; (progn
					; (format t "One of the closest possible solution is ~20F ~%" x1)
					; (setf flag 0)
					; (go end)
				; )
			; )			
			; ;;;;;;;;;;;;;;;;;;;;
			; ((< (abs y2) tolerance) 
			; (progn
				; (format t "One of the closest possible solution is ~20F ~%" x2)
				; (setf flag 0)
				; (go end)
			; )	)
			;;;;;;;;;;;;;;;;;;;;;
			
			
			;;;; this is the second condition which is also the rest condition
			(t 
			(progn
			
				;;;; this if statement handles the division by zero error
				(if (= y1 y2)
					(setq y1 (+ y1 tolerance))
				) 	
				
				;;; we calculate values of new x and y by using reguala falsi formula
				(setf xval (FalsiFormula x1 x2 y1 y2))
				(setf yval (PolyFun a b c d e f xval))
				
				;;;; this if statement checks whether new y is close to the tolerance or not..
				;;;; if yes, then prints the closest solution and sets the flag and goes to end
				;;;; if not, then compares the product of ylow and y new. if product is positive then we adjust the lower limiit. if not positive then we adjust the higher limit
				(if (< (abs yval) tolerance)
					(progn
						(format t " ~% One of the closest possible solution is ~10F ~%" xval)
						(setf flag 0)
						(go end)
					)
					(progn
						;;; if product positive, adjust lower limit
						;;; else adjust upper limit
						(if(> (* yval y1) 0)
							(progn
								(setf x1 xval)
								(setf y1 yval)
							)
							(progn
								(setf x2 xval)
								(setf y2 yval)
							)
						)
						
						;;;;; if still solution not found, then set the flag to recurse and repeat the process with adjusted limit
						(setf flag 1)
						(go end)
					)
				)
			)	
			;;;;;;;;;;;;;;;;;;;;;
			; (t 
				; (progn
					; (setf x1 (+ x1 0.01000000000000000000000000000000))
					; (setf flag 1)
					; (go end)
				; )	
			) 
		)
	end
	;(format t "Count ~f ~% " count)
	)
	
	;;;; this is the place where tail recursion occurs
	(if (= flag 1)
		(regula_falsi a b c d e f x1 x2 y1 y2)
	)
)

;;;;; this function calculates the values of generic function at a given point p
(defun PolyFun(a b c d e f p)
	 (float(+ (float(* a p p p p p)) (float(* b p p p p)) (float(* c p p p)) (float(* d p p)) (float(* e p)) f))
)

;;;; given the xlow, xhigh, ylow, and yhigh, this function calculates a new x value by using the regula falsi formula  
(defun FalsiFormula(xlow xhigh ylow yhigh)
	(float(/ (- (float(* xhigh ylow))(float(* xlow yhigh))) (float(- ylow yhigh))))	
)