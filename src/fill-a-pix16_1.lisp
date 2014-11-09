; GRUPO 17
; 
; Bruno Oliveira
; 67371
;
; Daniel da Costa
; 69720
;
; Francisco Maria Calisto
; 70916

;;(compile-file "exemplos.lisp")    
(load "exemplos.fas")       




(defstruct restricao  
    (listvars NIL)
    (pred NIL)
)


(defparameter myhashtable (make-hash-table :test 'equal))
(defparameter hashtable2 (make-hash-table :test 'equal))


(defun cria-restricao (lstvars predi)
	(let ((x NIL) )
	(
		setf x (make-restricao :listvars lstvars :pred predi)
	)	
	x)
)


(defun restricao-variaveis (r) 
	(
		restricao-listvars r
	)
)


(defun restricao-funcao-validacao (r)
	(
		restricao-pred r
	)
)

(defun conjuntos-iguais-p (bag1 bag2)
  (let ((table (make-hash-table :test 'equal)))
    (loop for key in bag1 do (incf (gethash key table 0)))
    (loop for key in bag2 do (decf (gethash key table 0)))
    (loop for val being each hash-value of table always (= val 0))))

(defstruct PSR
    (listvars NIL)
    (listdomains NIL)
    (listconstrains NIL)
    
    (listvalues NIL)
)


(defun cria-psr (varsl domainsl constrainsl)
	(let ((maxsz (list-length varsl)) 
				(valuesl NIL)
				(x NIL)
				)
			(dotimes (i maxsz varsl)
				(setf (gethash (nth i varsl) myhashtable) NIL)
				
				)
						
				(dotimes (i maxsz varsl)
				(setf valuesl (append valuesl (list NIL)))
				)
				
			(setf x (make-PSR
						:listvars varsl
						:listdomains domainsl
						:listconstrains constrainsl
						:listvalues valuesl))
	x					
	)		
	
)


(defun listOfPairsBuild (lst1 lst2)
  (loop for idx from 0
        for item in lst1
        collect (cons item (nth idx lst2))))


(defun psr-atribuicoes (psr)
	(let ((maxsz (list-length (PSR-listvars psr)))
		  (answ NIL)
		  (answnnil NIL)
		 ;; (tst 0)
		  	
					
				)
				
				(setq answ (listOfPairsBuild (PSR-listvars psr) (PSR-listvalues psr)))
				
				(dotimes (i maxsz (PSR-listvars psr))
					(if (not (equal (cdr (nth i answ)) NIL) )
						(setf answnnil (append answnnil (list (nth i answ))))
				)
		
	)
	(cond
	     ( (equal (list-length answnnil) 0 ) NIL)
	       (T answnnil) )
	
	
))



(defun psr-variaveis-todas (psr)
	(PSR-listvars psr)
)


(defun psr-variaveis-nao-atribuidas (psr)
	(let ((maxsz (list-length (PSR-listvars psr)))
		  (lst NIL))
		(dotimes (i maxsz (PSR-listvars psr))
			(if (equal (nth i (PSR-listvalues psr)) NIL)
				(setf lst (append lst (list (nth i (PSR-listvars psr)))))
			)
		)
		lst
	)
)


(defun psr-variavel-valor (psr var)
(let ((maxsz (list-length (PSR-listvars psr)))
      (answ NIL) 
       
     )
      ;;(defvar answ) 
       (dotimes (i maxsz (PSR-listvars psr))
            (if (equal (nth i (PSR-listvars psr)) var)
			(setf answ (nth i (PSR-listvalues psr)))
            			)
            )
    answ	    
    )
      
)



(defun psr-variavel-dominio (psr key)
    (defparameter *myhashtable* (make-hash-table :test 'equal))
    (let ( (maxsz (list-length (PSR-listvars psr)))
           (answ2 NIL)
     )
	;;(defvar answ2)
       (dotimes (i maxsz (PSR-listvars psr))
	    (if (equal (nth i (PSR-listvars psr)) key)
		
	    (setf answ2 (nth i (PSR-listdomains psr)))
     
            
            )
    )
    answ2
    )
    ;;(print "astrards")
    ;;(gethash key *myhashtable*)
    ;;(print "astrar9ds")
)


(defun psr-variavel-restricoes (psr key)
    (defparameter *myhashtable* (make-hash-table :test 'equal))
    (let ((maxsz (list-length (PSR-listconstrains psr)))
          (answ3 NIL) 
     			)
	
	(setf answ3 NIL)
	;;(print (list-length (PSR-listconstrains psr)))
       
       (dotimes (i maxsz (PSR-listconstrains psr))
            (if (not (equal (member key (restricao-listvars (nth i (PSR-listconstrains psr))) :test 'equal ) NIL))
					(setf answ3 (append answ3 (list (nth i (PSR-listconstrains psr)))))
					;;(print (restricao-listvars (nth i (PSR-listconstrains psr))))
            
            )
            
            )
	    answ3
    )
)



(defun psr-adiciona-atribuicao! (psr var val)
	(let ((maxsz (list-length (PSR-listvars psr))) )
				(setf (gethash var myhashtable)
							val)
				(dotimes (i maxsz );;(PSR-listvars psr))
					(if (equal (nth i (PSR-listvars psr)) var)
						(setf (nth i (PSR-listvalues psr)) val)
					)
				)
	)
)



(defun psr-remove-atribuicao! (psr var) 
	(let ((maxsz (list-length (PSR-listvars psr))) )

				(dotimes (i maxsz (PSR-listvars psr))
					(if (equal (nth i (PSR-listvars psr)) var) 
						(setf (nth i (PSR-listvalues psr)) NIL)
					)
				)
	)
)



(defun psr-altera-dominio! (psr var dom)
	(let ((maxsz (list-length (PSR-listvars psr))) )
		(dotimes (i maxsz (PSR-listvars psr))
			(if (equal (nth i (PSR-listvars psr)) var) 
				(setf (nth i (PSR-listdomains psr)) dom)
			)
		)
	)
)


(defun psr-completo-p (psr)
	(let ((maxsz (list-length (PSR-listvars psr)))
	       ;;(flagged 1)
	       (auxLi NIL)
	       (resultado 0) )
	     
		(setq auxLi (listOfPairsBuild (PSR-listvars psr) (PSR-listvalues psr)))
		
		(dotimes (i maxsz (PSR-listvars psr))
					(if (not (equal (cdr (nth i auxLi)) NIL) )
						(incf resultado)
				)
		)
	(cond
	     ( (equal (list-length auxLi) resultado ) T)
	       (T NIL) )	
	))

(defun psr-consistente-p (psr)

	(block alpha
    (let ((maxsz (list-length (PSR-listconstrains psr)))
	    (cont 0)
	    (ans T)
	      (flag T))
	     (defvar fun)
	    (dotimes (i maxsz )
		    (incf cont)
			;;(print i)
			    (setq fun (restricao-pred (nth i (PSR-listconstrains psr))) )
			    
			    (setf ans (funcall fun psr))
			    (if (equal ans NIL)
					
						;;(setf flag NIL)
						(return-from alpha (values-list (list NIL cont)))
					
					
			    
				    )
		    
			    
	    )
	    ;;(print "reached return")
	   (return-from alpha (values-list (list flag cont)))
			
    )
))

;;(psr-variavel-restricoes psr var)

(defun psr-variavel-consistente-p (psr var)
    (block alpha
    (let ((maxsz (list-length (psr-variavel-restricoes psr var)))
	    (cont 0)
	    (ans T)
	      (flag T))
	     (defvar fun)
	     
	     ;;(print maxsz)
	    (dotimes (i maxsz )
		    (incf cont)
			;;(print i)
			    (setq fun (restricao-pred (nth i (psr-variavel-restricoes psr var))) )
			    
			    
			    (setf ans (funcall fun psr))
			    (if (equal ans NIL)
					
						;;(setf flag NIL)
						(return-from alpha (values-list (list NIL cont)))
					
					
			    
				    )
		    
			    
	    )
	    ;;(print "reached return")
	 (return-from alpha (values-list (list flag cont)))		
    )
   
    )

)

(defun psr-variavel-consistente-pVALOR (psr var)
    (block alpha
    (let ((maxsz (list-length (psr-variavel-restricoes psr var)))
	    (cont 0)
	    (ans T)
	      )
	     (defvar fun)
	     
	     ;;(print maxsz)
	    (dotimes (i maxsz )
		    (incf cont)
			;;(print i)
			    (setq fun (restricao-pred (nth i (psr-variavel-restricoes psr var))) )
			    
			    
			    (setf ans (funcall fun psr))
			    (if (equal ans NIL)
					
						;;(setf flag NIL)
						(return-from alpha cont)
					
					
			    
				    )
		    
			    
	    )
	    ;;(print "reached return")
	 (return-from alpha cont)		
    )
   
    )

)




(defun psr-atribuicao-consistente-p (psr var val)
	(let (;;(auxi NIL)
		 (flag T)
		 (oldval (psr-variavel-valor psr var)) 
		 	)
	
	;;(print "as")
	(psr-adiciona-atribuicao! psr var val) 
	;;(print "---oldval---")
	;;(print oldval)
	;;(print "---End Of oldval---")
	;;(print "asd")
	;;(print (psr-variavel-consistente-p psr var))
	;;(print "dsa")
	
	(if (equal (psr-variavel-consistente-p psr var) NIL)
			(setf flag NIL)
	)
	;;(print "Flag value after 1st if")
	;;(print flag)
	;;(print "Ends flag debug")
	;;(if (equal flag NIL)
		(psr-adiciona-atribuicao! psr var oldval) 
	;;)
	;;(print (psr-variavel-valor psr var))
	;;(print "LEL")
	
	(values flag (psr-variavel-consistente-pVALOR psr var))
	;;(values-list (list flag (psr-variavel-consistente-pVALOR psr var)))
))

(defun psr-consistente-pAuxiliar (psr auxList)

	(block alpha
    (let ((maxsz (list-length auxList))
	    (cont 0)
	    (ans T)
	      (flag T))
	     (defvar fun)
	     
	    ;; (print maxsz)
	    (dotimes (i maxsz )
		    (incf cont)
			;;(print i)
			    (setq fun (restricao-pred (nth i auxList)) )
			    
			    ;;(print "Inside consistente Aux")
			    ;;(print psr)
			     ;;(print "Out consistente Aux")
			    (setf ans (funcall fun psr))
			    
			   
			    
			    (if (equal ans NIL)
					
						;;(setf flag NIL)
						(return-from alpha (values-list (list NIL)))
					
					
			    
				    )	    
	    )
	    ;;(print "reached return")
	   (return-from alpha (values-list (list flag)))
			
    )
))

(defun psr-consistente-pAuxiliarValues (psr auxList)

	(block alpha
    (let ((maxsz (list-length auxList))
	    (cont 0)
	    (ans T))
	     ;; (flag T))
	     (defvar fun)
	     
	     ;;(print maxsz)
	    (dotimes (i maxsz )
		    (incf cont)
			;;(print i)
			    (setq fun (restricao-pred (nth i auxList)) )
			    
			    
			    (setf ans (funcall fun psr))
			    (if (equal ans NIL)
					
						;;(setf flag NIL)
						(return-from alpha (values-list (list cont)))
					
					
			    
				    )
		    
			    
	    )
	    ;;(print "reached return")
	   (return-from alpha (values-list (list cont)))
			
    )
))

(defun psr-atribuicoes-consistentes-arco-p (psr var1 val1 var2 val2)
    (let (
	     ;;(flag T)
	     (oldval1 (psr-variavel-valor psr var1)) 
	     (oldval2 (psr-variavel-valor psr var2))
	     (ansDebug NIL)
	     (novopsr NIL)
	)
	
	(setf novopsr (cria-psr (PSR-listvars psr) (PSR-listdomains psr) (PSR-listconstrains psr)))
	(psr-adiciona-atribuicao! psr var1 val1)
	(psr-adiciona-atribuicao! psr var2 val2)
	(psr-adiciona-atribuicao! novopsr var1 val1)
	(psr-adiciona-atribuicao! novopsr var2 val2)
	
	
	
	
	;;(print "Before intersection")
	
	;;(print (psr-variavel-valor psr var1))
	;;(print (psr-variavel-valor psr var2))
	
	(setq ansDebug (intersection (psr-variavel-restricoes psr var1)
		    (psr-variavel-restricoes psr var2)))
    ;;(print ansDebug)		    
	;;(print "Ended intersection -- ")
	
	;;(print (psr-consistente-pAuxiliar psr ansDebug))
	;;(print (psr-consistente-pAuxiliarValues psr ansDebug))
	;;(print (values (psr-consistente-pAuxiliar psr ansDebug) (psr-consistente-pAuxiliarValues psr ansDebug))  )  
	    
	(psr-adiciona-atribuicao! psr var1 oldval1) 
	(psr-adiciona-atribuicao! psr var2 oldval2)  
	
	(values (psr-consistente-pAuxiliar novopsr ansDebug) (psr-consistente-pAuxiliarValues novopsr ansDebug))
)
)

(defun GetNeighboursOfArrayCell (arr i j)
	(let (
		(listaVizinhos NIL)
		(listaVizinhosAux NIL)
		(listaVizinhosAns NIL)
		
		(pairv (cons i j))
		)
		
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (- j 1)) hashtable2))))
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) j) hashtable2))))
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (+ j 1)) hashtable2))))
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (- j 1)) hashtable2))))
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (+ j 1)) hashtable2))))
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (- j 1)) hashtable2))))
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) j) hashtable2))))
		(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (+ j 1)) hashtable2))))
		
		(incf i)
		(incf j)
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr (- i 1) (- j 1)))))
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr (- i 1) j))))
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr (- i 1) (+ j 1)))))
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr i (- j 1)))))
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr i (+ j 1)))))
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr (+ i 1) (- j 1)))))
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr (+ i 1) j))))
		(setf listaVizinhosAux (append listaVizinhosAux (list (aref arr (+ i 1) (+ j 1)))))
		
		
		
		
		
		
	
		
		(dotimes (tt 8)
			
			(if (not (equal -1 (nth tt listaVizinhosAux))) 
				(setf listaVizinhosAns (append listaVizinhosAns (list (nth tt listaVizinhos))))
			))
		(setf listaVizinhosAns (append listaVizinhosAns (list (gethash (cons (car pairv) (cdr pairv)) hashtable2))))
          
	listaVizinhosAns	
	)
)

(defun CriaPredicado (arr i j)
	(incf i)
	(incf j)
	(let(
		(listaviz (GetNeighboursOfArrayCell arr (- i 1) (- j 1)))
	       (valor (aref arr i j)) )  

	(setq fun #'(lambda (psr)
	(let ( 
	       (nzeros 0)
	       (soma 0)
	       (sumaux 0 ) )
	       
	     (dotimes (i (list-length listaviz) )
				
			#|	(print "The ith neighbour and value are")
				(print (nth i listaviz))
				(print (psr-variavel-valor psr (nth i listaviz)))
				(print "OUTPUT SUCCESSFUL") |#
	     
				(if (equal (psr-variavel-valor psr (nth i listaviz)) 0) 
					(incf nzeros)
				)
	     )
	     
	    
		#|(print ":::::::::::::::::::::::::::::::::::::::::::::")
		(print nzeros)
		(print "===============================================")
		
		(print (array-dimension arr 0))
		(print "===============After asking for dim===================")|#
	     
	     (dotimes (i (list-length listaviz) )
			(if (not (equal NIL (psr-variavel-valor psr (nth i listaviz) )) )
				(setf soma (+ soma (psr-variavel-valor psr (nth i listaviz) )))
		    )
	     )
	     
	              
	
	 (setf sumaux soma) (if (and (<= sumaux valor) (>= (- 9 nzeros) valor)) T ))))
	 
	fun
	))	

(defun fill-a-pix->psr (arr)
	(let (
		(cont 0)
	   (lines (array-dimension arr 0))
	   (columns (array-dimension arr 1))
	   (listadevars NIL)
	   (listadedoms NIL)
	   (listaderestr NIL)
		 )
		 (defparameter aux (make-array (list (+ 2 lines) (+ 2 columns) )))
		 
		 ;; Sets aux array to only -1 
		 (dotimes (i (+ 2 lines))
			(dotimes (j (+ 2 columns))
				
				(setf (aref aux i j) -1)
			)
		)
		
		(dotimes (i lines)
			(dotimes (j columns)
				
				(setf (aref aux (+ 1 i) (+ 1 j)) (aref arr i j))
				(setf (gethash (cons i j) hashtable2) (write-to-string cont))
				(setf listadevars (append listadevars (list (write-to-string cont))))
				
				(if (not (equal NIL (aref arr i j) ))
				(setf listaderestr (append listaderestr (list (cria-restricao (GetNeighboursOfArrayCell aux i j) (CriaPredicado aux i j) )) ) )
				)
				(incf cont)
			)
		)
		
		(setf listadedoms (make-list cont :initial-element '(0 1)))
		
		(cria-psr listadevars listadedoms listaderestr)
		
		)
)

(defun psr->fill-a-pix (psr lin col)
	(defparameter aux (make-array (list lin col )))
	(dotimes (i lin)
			(dotimes (j col)
				(setf (aref aux i j) (psr-variavel-valor psr (write-to-string i)) )
			)
	)
	aux
)

(defun procura-retrocesso-simples (psr)
  (cond ((notany #'zerop psr)
     (if (psr-consistente-p psr)
         psr
         nil))
    (t (let ((positions (fill-a-pix->psr psr)))
         (loop for position in positions
          do (loop for number in '(1 2 3 4 5 6 7 8 9)
              do (let ((result (procura-retrocesso-simples
                        (psr->fill-a-pix psr
                            lin
                            col))))
                   (when result
                 (return-from procura-retrocesso-simples result)))))))))