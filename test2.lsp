
(defun f(x)(+ (* x x x) (* -9 x ) 3 ))


(defun regula_falsi(x1 x2 tolerance iteration)	
(if (eq iteration 0) 		
	x1		
	(progn(setq shadow (f x1))			
	(format t "~%~,10F	~,10F ~,10F ~D" x1 x2 shadow iteration)			
	(if (and (< shadow tolerance) (> shadow (* tolerance -1)))				
	(progn (print "tolerance alcancada") chute )				
	(progn 					
	;encontra a raiz da secante entre os pontos do intervalo x1 x2					
	(setq chute (/(-(*(read-from-string (format NIL "~20,20F" x1))(funcall f x2)) (* x2 (funcall f x1)))(-(funcall f x2)(funcall f x1))) )					
	(if (> (f chute) 0) (setq x1 chute) (setq x2 chute))					
	(regula_falsi x1 x2 tolerance (- iteration 1))			
	)			
	)	
	)
)
)

; exemplo de utilização;
;(print (regula_falsi #'f 0 1 0.00001 20 ))