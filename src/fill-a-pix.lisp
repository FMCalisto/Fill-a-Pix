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

(load "exemplos.fas")

; ======================================================================================= ;
;                               DEFINICAO DAS ESTRUTURAS DE DADOS                         ;
; ======================================================================================= ;


(defstruct restricao 
    (listvars NIL)
    (pred NIL)
)

; HASHTABLE GLOBAL

; The variables of our PSR will serve to index the Hashtable
; and this way we can access the respective values at a lower cost.

; Given a place in the game board this returns the variable
; associated with that place:
(defparameter myhashtable (make-hash-table :test 'equal))
(defparameter hashtable2 (make-hash-table :test 'equal))

; Given a variable speeds up the retrieve of its value:
(defparameter hashtablevals (make-hash-table :test 'equal))

; Speeds up domain retrieval:
(defparameter hashtabledoms (make-hash-table :test 'equal))

; Speeds up constrains checking:
(defparameter hashtableconstr (make-hash-table :test 'equal))

; ======================================================================================= ;
; ======================================================================================= ;
;                                                                                         ;
;                                  PROJECT | FIRST PART                                   ;
;                                                                                         ;
; ======================================================================================= ;
; ======================================================================================= ;


; ======================================================================================= ;
;                                        TIPO RESTRICAO                                   ;
; ======================================================================================= ;

; The restriction type has as arguments a list of variables and a predicate that evaluates
; whether a CSP is valid or not.
; The predicate will have to call the function check-CSP so that build the constraint.

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


; ======================================================================================= ;
;                                              PSR                                        ;
; ======================================================================================= ;

; The Satisfaction Problem Constrained (PSR) type is used to represent
; one problem satisfaction of restrictions, which stores information about the variables,
; domains and restrictions of a Satisfaction Problem of Constrained (PSR).

; USED LISTS

; listvars       - Variables list;
; listdomains    - Domains list;
; listconstrains - Constrains list;
; listvalues     - Assignment List.

(defstruct PSR
    (listvars NIL)
    (listdomains NIL)
    (listconstrains NIL)
    
    (listvalues NIL)
)

; CRIA PSR

; This constructor receives a variaeis list, a list of domains
; with the same size of the variable list, and a list of restrictions.
; Associated variables and fields in a Hashtable which easily builds
; structures and return NIL.

(defun cria-psr (varsl domainsl constrainsl)
	(let ((maxsz (list-length varsl))
			(maxsz2 (list-length constrainsl))
				(valuesl NIL)
				(x NIL)
				(answ3 NIL)
				)
			(dotimes (i maxsz varsl)
				(progn
					(setf (gethash (nth i varsl) myhashtable) NIL)
					(setf (gethash (nth i varsl) hashtablevals) i)
					(setf (gethash (nth i varsl) hashtabledoms) i)
					(setf (gethash (nth i varsl) hashtableconstr) NIL)
				)
			)
						
				(dotimes (i maxsz varsl)
				(setf valuesl (append valuesl (list NIL)))
				)
				
			(setf x (make-PSR
						:listvars varsl
						:listdomains domainsl
						:listconstrains constrainsl
						:listvalues valuesl))
			
			(dotimes (j maxsz varsl)
				(dotimes (i maxsz2 constrainsl)
					(progn
						(if (not
									(equal
										(member (nth j varsl)
											(restricao-variaveis (nth i constrainsl))
											:test
											'equal
										)
										NIL
									)
								)
							(setf answ3 (append answ3 (list (nth i constrainsl))))
						)
					)
				)
				(setf (gethash (nth j varsl) hashtableconstr) answ3)
				(setf answ3 NIL)
			)
	x					
	)	
)

; ATRIBUICOES DO PSR

; Receives a PSR, and returns a list of all the PSR assignments.
; In this function we use a function aux to see if we build
; a list of pairs.

(defun listOfPairsBuild (lst1 lst2)
  (loop for idx from 0
  	for item in lst1
  		collect (cons item (nth idx lst2))
  )
)

(defun psr-atribuicoes (psr)
	(let
		(
			(maxsz (list-length (PSR-listvars psr)))
		  (answ NIL)
		  (answnnil NIL)					
		)
				
		(setq
			answ
			(listOfPairsBuild (PSR-listvars psr) (PSR-listvalues psr))
		)
				
		(dotimes (i maxsz (PSR-listvars psr))
			(if (not (equal (cdr (nth i answ)) NIL) )
				(setf answnnil (append answnnil (list (nth i answ))))
			)
		)

		(cond
			((equal (list-length answnnil) 0 ) NIL)
				(T answnnil)
		)	
	)
)

; VARIAVEIS PSR

; Receives a PSR and returns a list of all the PSR variables,
; whether or not assigned.

(defun psr-variaveis-todas (psr)
	(PSR-listvars psr)
)

; VARIAVEIS NAO ATRIBUIDAS

; Receives a PSR and returns a list of the variables that PSR
; still have not been assigned.

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

; VALOR DA VARIAVEL

; Selector receives a PSR and a variable.
; If the variable have an assigned value, and return the corresponding value.
; If the variable is not assigned, and returned NIL.

(defun psr-variavel-valor (psr var)
		(let
			(
				(ans NIL)
			)
      (setf ans (gethash var hashtablevals))
      (nth ans (PSR-listvalues psr))
      )
)

; DOMINIO DA VARIAVEL

; Returns the corresponding domain to that variable.

(defun psr-variavel-dominio (psr key)
		(let ( (ans NIL)
		
				)
				
		(setf psr psr)
      (setf ans (gethash key hashtabledoms))
      (nth ans (PSR-listdomains psr))
      )
)

; RESTRICOES DA VARIAVEL

; Returns a list of all restrictions applicable to this variable.

(defun psr-variavel-restricoes (psr key)
    (let ( (ans NIL)
		
				)
				
		(setf psr psr)
      (setf ans (gethash key hashtableconstr)) 
      ans
      )
)


; ADICIONA ATRIBUICAO

; This modifier receives an PSR, a variable and a value,
; and changes the PSR received, assigning the value to variable.
; If the variable already had been assigned, the new value replaces the previous value.
; The return value of this function is not defined.

(defun psr-adiciona-atribuicao! (psr var val)
	(setf psr psr)
	(setf (nth (gethash var hashtablevals) (PSR-listvalues psr)) val)
)

; REMOVE ATRIBUICAO

; This modifier receives a PSR and a variable,
; and changes the PSR received, removing any
; attribution the variable that has been done previously.
; The variable effectively becomes not be assigned.
; The return value of this function is not defined.

(defun psr-remove-atribuicao! (psr var)
	(setf psr psr)
	(setf (nth (gethash var hashtablevals) (PSR-listvalues psr)) NIL)
)

; ALTERA DOMINIO

; This modifier receives an RSP, a variable,
; and domain and changes the PSR received,
; changing the domain associated with the received variable,
; so that it will be the domain received.
; The return value of this function is not defined.

(defun psr-altera-dominio! (psr var dom)
	(let ((ans NIL)
				)
				
		(setf psr psr)
    (setf ans (gethash var hashtabledoms)) 
    (setf (nth ans (PSR-listdomains psr)) dom)
  )
)

; PSR COMPLETO

; Receives a PSR and returns T if it's complete,
; i.e. if you have an attribution for all variables.
; Returns NIL otherwise.

(defun psr-completo-p (psr)
	(let
		(
			(maxsz (list-length (PSR-listvalues psr))) (nilelems 0)
		)
		(dotimes (i maxsz)
			(if (equal (nth i (PSR-listvalues psr)) NIL) 
				(incf nilelems)
			)
		)
		(and
			(=
				(length (PSR-listvars psr))
				(length (PSR-listvalues psr))
			)
			(= 0 nilelems)
		)
	)
)

; PSR CONSISTENTE

; 1) The logic value that T and the PSR are consistent,
;    that is, if all of the PSR restrictions are met
;    to existing assignments, and NIL cc.

; 2) Number of consistency tests needed to calculate
;    the consistency.

(defun psr-consistente-p (psr)
	(if (null (PSR-listconstrains psr)) (values T 0)
		(let
			(
				(tests 0)
				(result T)
			)
			(dolist (i (PSR-listconstrains psr)) 
				(if (null (funcall (restricao-funcao-validacao i) psr)) 
					(progn
						(setf result nil)
						(return)
					)
				)
				(incf tests)
			)
			(if result
				()
				(incf tests)
			)
			(values result tests)
		)
	)
)


; PSR VARIAVEL CONSISTENTE

; 1) logical value that and T if the variable is consistent
;    taking into account the PSR assignments.

; 2) Return corresponds to the number of test
;    consistency that were required to
;    determining consistency.

(defun psr-variavel-consistente-p (psr var)
	(let
		(
			(count 0)
			(restr (psr-variavel-restricoes psr var))
		)			
		(cond
			(
				(null restr) 
				(return-from psr-variavel-consistente-p (values T 0))
			)
		)
		(dolist (ele restr)
			(incf count)
			(when
				(not
					(funcall
						(restricao-funcao-validacao ele)
						psr
					)
				)	
				(return-from
					psr-variavel-consistente-p (values nil count)
				)
			)
		)
	(values T count)
	)
)


; PSR ATRIBUICAO CONSISTENTE

; This test receives an RSP, a variable and a value,
; and returns 2 values.

(defun psr-atribuicao-consistente-p(psr var valor)
	(if (equal (psr-listconstrains psr) nil)
		(return-from
			psr-atribuicao-consistente-p
				(values T 0)
		)
	)
	
	(let
		(
			(res nil)
			(aux (psr-variavel-valor psr var))
		)
		(cond
			(
				(equal aux nil) 
				(setf aux nil)
			)
		)
		
		(psr-adiciona-atribuicao! psr var valor)
		
		(setf
			res
			(multiple-value-list
				(psr-variavel-consistente-p psr var)
			)
		)
		
		(psr-adiciona-atribuicao! psr var aux)
		
		(return-from
			psr-atribuicao-consistente-p
				(values (nth 0 res) (nth 1 res))
		)
	)
)


; MY MEMBER

; Verify if an elem is in the list.

(defun my-member (elem lista)
	(cond ((null lista) nil)
		((equal elem (first lista)) T)
		(T (my-member elem (rest lista)))))


; PSR ATRIBUICOES CONSISTENTES EM ARCO

; This test receives a PSR, a variable v1,
; and a value for this variable, a variable v2 and a
; value for that variable, and returns two values.

(defun psr-atribuicoes-consistentes-arco-p (psr var1 v1 var2 v2)
	(if (equal (psr-listconstrains psr) nil)
		(return-from psr-atribuicoes-consistentes-arco-p (values T 0))
	)
	(let ((testes 0) (aux1 (psr-variavel-valor psr var1)) (aux2 (psr-variavel-valor psr var2)))
		(if (equal aux1 nil) (setf aux1 nil))
		(if (equal aux2 nil) (setf aux2 nil))
		(psr-adiciona-atribuicao! psr var1 v1)
		(psr-adiciona-atribuicao! psr var2 v2)
		(dolist (r (psr-variavel-restricoes psr var1))
			(cond ((my-member var2 (restricao-variaveis r))
					(incf testes)
					(cond ((not (funcall (restricao-funcao-validacao r) psr))
						(psr-adiciona-atribuicao! psr var1 aux1)
						(psr-adiciona-atribuicao! psr var2 aux2)
						(return-from psr-atribuicoes-consistentes-arco-p (values nil testes))
						  )
				    )
				   )
			)
		)
		(psr-adiciona-atribuicao! psr var1 aux1)
		(psr-adiciona-atribuicao! psr var2 aux2)
		(values T testes)
	)
)


; ======================================================================================= ;
;                                    FUNCOES DE CONVERSAO                                 ;
; ======================================================================================= ;


; TURNS FILL-A-PIX IN PSR

; Receives a corresponding array to a two-dimensional table mxn of a puzzle
; Fill-a-Pix unresolved and returns a PSR representing the problem to solve
; this particular puzzle.
;
; 1) This function is found and placed all the neighbors of a variable in a
;    list of neighbors;
;
; 2) Create a predicate receiving an Array and their indices thereof to
;    size of calculating his own;
;
; 3) Assign the name of the variables equal to the indices and puts a list of strings
;    to be evaluated as a name;
;
; 4) Put all restrictions checked a list of restrictions,
;    where each constraint and enforced by a given predicate checking
;    all its neighbors around;
;
; 5) assign the domain 0 or 1 (white or black) in domains list
;    the corresponding variables;
;
; Finally creates the PSR with the arguments of the new variables list,
; domains list and restrictions list.

; GET NEIGHBOURS OF ARRAY CELL

(defun GetNeighboursOfArrayCell (arr i j)
	(let
		(
			(listaVizinhos NIL)
		)
		(cond ((and (= i 0) (= j 0))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (+ j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (+ j 1)) hashtable2)))))		
			((and (= i 0) (= j (- (array-dimension arr 1) 1) ))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) j) hashtable2)))))		
			((and (= i (- (array-dimension arr 0) 1))  (= j 0) )
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (+ j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (+ j 1)) hashtable2)))))		
			((and (= i (- (array-dimension arr 0) 1)) (= j (- (array-dimension arr 1) 1) ))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j) hashtable2)))))
			((= i 0)
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (+ j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (+ j 1)) hashtable2)))))
			((= i (- (array-dimension arr 0) 1))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (+ j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (- j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (+ j 1)) hashtable2)))))
			((= j 0)
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) j ) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (+ j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j ) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (+ j 1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) j ) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (+ j 1)) hashtable2)))))
			((= j (- (array-dimension arr 1) 1) )
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (- j  1)) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) j ) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (- j 1 )) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j ) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (- j 1 )) hashtable2))))
				(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) j ) hashtable2)))))
		(T 
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (- j 1)) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) j) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons (- i 1) (+ j 1)) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (- j 1)) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons i j) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons i (+ j 1)) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (- j 1)) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) j) hashtable2))))
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (+ j 1)) hashtable2))))
		)
		)
		listaVizinhos	
	)
)

; CRIA PREDICADO

(defun CriaPredicado (arr i j)
	(let
		(
			(listaviz (GetNeighboursOfArrayCell arr i j))
	    (valor (aref arr i j))
	    func
	  )
	  (setq
	  	func
	  	#'(lambda (psr)
					(let
						(
							(nones 0) 
				      (nnils 0)
				    ) 
				    (dotimes (i (list-length listaviz)) 
							(if (equal (psr-variavel-valor psr (nth i listaviz)) 1) 
								(incf nones)
							)
							(if (equal (psr-variavel-valor psr (nth i listaviz)) NIL) 
								(incf nnils)
							)
				    )
				    (if (and (<= valor (+ nones nnils)) (<= nones valor)) 
							T
						)
					)
				)
	  )
		func 
	)
)


; TURNS FILL-A-PIX TO PSR 

(defun fill-a-pix->psr (arr)
	(let
		(
			(cont 0)
	   	(lines (array-dimension arr 0))
	   	(columns (array-dimension arr 1))
	   	(listadevars NIL)
	   	(listadedoms NIL)
	   	(listaderestr NIL)
		)
		(defparameter aux (make-array (list lines columns )))

		; Place the array aux just -1
		(dotimes (i lines)
			(dotimes (j columns)	
				(setf
					(aref aux i j)
					(aref arr i j)
				)
				(setf
					(gethash
						(cons i j)
						hashtable2
					)
					(write-to-string cont)
				)
				(setf
					listadevars
					(append
						listadevars
						(list (write-to-string cont))
					)
				)
				(incf cont)
			)
		)

		(dotimes (i lines)
			(dotimes (j columns)		
				(if (not (equal NIL (aref arr i j)))
					(setf
						listaderestr
						(append
							listaderestr
							(list
								(cria-restricao
									(GetNeighboursOfArrayCell arr i j)
									(CriaPredicado arr i j)
								)
							)
						)
					)
				)
			)
		)		
	
		(setf
			listadedoms
			(make-list cont :initial-element '(0 1))
		)
		(cria-psr listadevars listadedoms listaderestr)
	)
)


; TURNS PSR TO FILL-A-PIX

; The PSR receives a fixed, an integer representing the number of lines l,
; and another that represents the number of columns c, and returns a two-dimensional array
; lin rows and col columns, containing for each position row/column in assigning
; the corresponding PSR variable.
; In this implementation was only necessary to create the name of the variable,
; which is the variable value of the index of our PSR in string.

(defun psr->fill-a-pix (psr lin col)
	(let
		(
			(aux (make-array (list lin col )))
		  (tvars 0)
		)
		(dotimes (i lin)
			(dotimes (j col)
				(setf
					(aref aux i j)
					(psr-variavel-valor psr (write-to-string tvars))
				)
				(incf tvars) 
			)
		)
		aux
	)
)


; ======================================================================================= ;
;                               PROCURA RETROCESSO SIMPLES                                ;
; ======================================================================================= ;

; PROCURA RETROCESSO SIMPLES

; Receives a PSR, and try to solve it using a search with simple regression,
; i.e. without using any heuristics to choose the next variable and value,
; and no inference mechanism.
; Returns two values, the first and the PSR resolved, or NIL if no solution exists.

(defun procura-retrocesso-simples (psr)
	(let
		(
			var 
			(resultado T)
			(consistency_checks 0)
			(isvalid NIL)
		)
		(if (psr-completo-p psr)
			(return-from
				procura-retrocesso-simples (values psr consistency_checks)
			)
		)
		(setf
			var
			(first
				(psr-variaveis-nao-atribuidas psr)
			)
		)
		(dolist (val (psr-variavel-dominio psr var))
			(multiple-value-bind
				(resultado test)
				(psr-atribuicao-consistente-p psr var val) 
				(setf
					isvalid
					resultado
				)
				(setf
					consistency_checks
					(+ consistency_checks test)
				)
			)
			(if isvalid
				(progn
					(psr-adiciona-atribuicao! psr var val)
					(multiple-value-bind
						(ret testes-feitos)
						(procura-retrocesso-simples psr)
						(setf
							resultado
							ret
						)
						(setf
							consistency_checks
							(+ consistency_checks testes-feitos)
						)
					)
					(if resultado
						(return-from
							procura-retrocesso-simples (values psr consistency_checks)
						)
					)
					(psr-remove-atribuicao! psr var)
				)
			)
		)
		(values NIL consistency_checks)
	)
)

; RESOLVE SIMPLES

; Takes an array with a Fill-a-Pix puzzle to be solved, try to solve it using the
; procura-retrocesso-simples function and returns the final result as an array
; from the Fill-a-Pix problem solved.
; If there is no solution, must be returned NIL.

(defun resolve-simples (arr) 
	(psr->fill-a-pix
		(procura-retrocesso-simples
			(fill-a-pix->psr arr)
		)
		(array-dimension arr 0)
		(array-dimension arr 1)
	)
)


; ======================================================================================= ;
; ======================================================================================= ;
;                                                                                         ;
;                                  PROJECT | SECOND PART                                  ;
;                                                                                         ;
; ======================================================================================= ;
; ======================================================================================= ;

; ======================================================================================= ;
;                                PROCURAS MAIS AVANCADAS                                  ;
; ======================================================================================= ;


; COUNT NON ATTRIBUTED VARIABLES IN CONSTRAINS

; This auxiliar function counts the number of variables that does not contain
; associated constrains.

(defun Count-Non-Attributed-Variables-In-Constraint (psr lst)
	(let
		(
			(counter 0)
			(sz (list-length lst))
		)
		(dotimes (i sz) 
			(if (equal NIL (psr-variavel-valor psr (nth i lst)) ) 
				(incf counter)
			) 
		)
		counter	
	)
)

; RE-COMPUTE DEGREE OF

; The objective of this funtion is to count the number of Total Degrees
; and see if we have at leat one Constrain attributed.

(defun ReComputeDegreeOf (psr var)
	(let
		(
			(lista1 (psr-variavel-restricoes psr var))
	    (totalDegree 0)
		)

		; nth k lista1 is a constraint
		
		(dotimes (k (list-length lista1))
			(progn
				(if (>
							(Count-Non-Attributed-Variables-In-Constraint
								psr
								(restricao-variaveis (nth k lista1))
							)
							1
						)
					(incf totalDegree)
				)
			)		 
		)
		totalDegree
	)	
)

; GRAU VARS NAO ATRIBUIDAS

; Calculate the Degree of non attributed variables.

(defun grauvarsnaoatrib (psr)
	(let ( 
			(lista (psr-variaveis-nao-atribuidas psr) )
			(sz (list-length (psr-variaveis-nao-atribuidas psr) ) )
			(myhashtable3 (make-hash-table :test 'equal))
			(lst1 NIL)
			(lst2 NIL)
			(finallst NIL)
		)
		
		(dotimes (i sz)
			(setf (gethash (nth i lista) myhashtable3)
				  (list-length (psr-variavel-restricoes psr (nth i lista)))
			)
		)
		
		(dotimes (i sz)
			 (setf lst1 (append lst1 (list (nth i lista))))
			 (setf lst2 (append lst2 (list (gethash (nth i lista) myhashtable3))))
		)
		
		(setq finallst (listOfPairsBuild lst1 lst2) )
		(stable-sort finallst #'> :key #'cdr)
	finallst	
	)
)

; RE-CALCULATE GLOBAL DEGREES

; It re-calculate the global degrees and sort it.

(defun ReCalculateGlobalDegrees (psr lst2) 
	(let ( (sz (list-length lst2) )
			)
	(dotimes (i sz)
		(setf
			(cdr (nth i lst2))
			(ReComputeDegreeOf psr (car (nth i lst2)))
		)
	)
	(stable-sort lst2 #'> :key #'cdr)
	lst2
	)
)

; PROCURA RETROCESSO GRAU

; The idea of this function is to backtrack search using Maximum Degree Heuristic
; implemented with support of auxiliar function:
; 
; ReCalculateGlobalDegrees - re-calculate the global degrees of the pair list;

(defun procura-retrocesso-grau (psr)
	(let
		(
			var 
			(resultado T)
			(consistency_checks 0)
			(isvalid NIL)
			(pairslst (grauvarsnaoatrib psr)) 
		)
			
		(ReCalculateGlobalDegrees psr pairslst)
		
		(if (psr-completo-p psr)
			(return-from procura-retrocesso-grau (values psr consistency_checks))
		)
		
		(setf
			var
			(car (first pairslst))
		)
		
		(dolist (val (psr-variavel-dominio psr var))
			(multiple-value-bind
				(resultado test)
				(psr-atribuicao-consistente-p psr var val) 
				(setf
					isvalid
					resultado
				)
				(setf
					consistency_checks
					(+ consistency_checks test)
				)
			)
			(if isvalid
				(progn
					(psr-adiciona-atribuicao! psr var val)
					(multiple-value-bind
						(ret testes-feitos)
						(procura-retrocesso-grau psr)
						(setf
							resultado
							ret
						)
						(setf
							consistency_checks
							(+ consistency_checks testes-feitos))
						)
					(if resultado
						(return-from procura-retrocesso-grau (values psr consistency_checks))
					)
					(psr-remove-atribuicao! psr var)
					(ReCalculateGlobalDegrees psr pairslst)
				)
			)
		)
		(values NIL consistency_checks)
	)
)


; INFERENCIA

; Inference function is a list of inferences.

(defstruct inferencia 
	(lista nil)
)

; MINIMUM REMAINING VALUES

; Search for smaller domains and selects the variable with the smallest domain.

(defun min-remaining-values (psr)
	(let
		(
			(varList (psr-variaveis-nao-atribuidas psr)) 
			(select-var nil)
			(minimum-domain-size nil)
			(aux 0)
		)
		(setf select-var (first varList))
		(setf minimum-domain-size (length (psr-variavel-dominio psr select-var)))
		(setf varList (rest varList))
		(dolist (i varList)
			(setf aux (length (psr-variavel-dominio psr i)))
			(cond ((< aux minimum-domain-size)
					(setf select-var i)
					(setf minimum-domain-size aux)
				  )
			)
		)
	select-var)
)

; ADD INFERENCES

; Add inferences to our psr.

(defun AddInferences (psr inferencias)
	(let
		(
			(lista (inferencia-lista inferencias))
			(dom nil)
		)
		(dolist (i lista)
			(setf dom (psr-variavel-dominio psr (car i)))
			(psr-altera-dominio! psr (car i) (cdr i))
			(setf (cdr i) dom)
		)
	)		
)

; GET INFERENCE DOMAIN

; Return the Domain of an inference associated to some variable.

(defun GetInferenceDomain (var inferencias)
	(let
		(
			(lista (inferencia-lista inferencias))
	    (flag -1) 
	  )
		(dolist (i lista)
			(cond ((equal var (car i))
					(return-from GetInferenceDomain (cdr i)))))
		flag
	)
)


; UPDATE INFERENCE DOMAIN

; Update the Inference Domain with a new one.

(defun UpdInferenceDomain(var dominio inferencias)
	(let ( (lista (inferencia-lista inferencias))
	     )
		(dolist (i lista)
				(cond ((equal var (car i))
						(setf (cdr i) dominio)
						(return-from UpdInferenceDomain))))
		(setf
			(inferencia-lista inferencias)
			(cons
				(cons var dominio)
				(inferencia-lista inferencias)
			)
		)
	)
)


; REVISE

; Successful attempt for making x and y arc consistent.

(defun revise(psr x y inferencias)
	(let ((testesTotais 0) 
	      (revised nil) 
	      (dominio-x nil) 
	      (dominio-y nil) 
	      (novo-dominio-x nil) 
	      (foundConsistentValue nil) 
	      (aux nil)
	     )
		(setf aux (GetInferenceDomain x inferencias))
		(if (not (equal aux -1)) (setf dominio-x aux)
				  (setf dominio-x (copy-list (psr-variavel-dominio psr x)))
		)
		(setf novo-dominio-x dominio-x)
		(setf aux (GetInferenceDomain y inferencias))
		(if (psr-variavel-valor psr y) (setf dominio-y (list (psr-variavel-valor psr y)))
			(if (not (equal aux -1)) (setf dominio-y aux)
						(setf dominio-y (copy-list (psr-variavel-dominio psr y)))
			)
		)
		(dolist (vx dominio-x)
			(setf foundConsistentValue nil)
			(dolist (vy dominio-y)
				(setf aux (multiple-value-list (psr-atribuicoes-consistentes-arco-p psr x vx y vy)))
				(setf testesTotais (+ testesTotais (nth 1 aux)))
				(cond ((nth 0 aux)
					(setf foundConsistentValue T) (return))))
			(cond ((not foundConsistentValue)
				(setf revised T)
				(setf novo-dominio-x (remove vx novo-dominio-x :test #'equal)))))
		(if (equal revised T)
			(UpdInferenceDomain x novo-dominio-x inferencias)
			)
	(values revised testesTotais)
	)
)


; ARCOS VIZINHOS NAO ATRIBUIDOS

; Search for non assigned neighbors in arc.

(defun arcos-vizinhos-nao-atribuidos(psr var)
	(let
		(
			(result nil)
		)
		(dolist (var-natribuida (psr-variaveis-nao-atribuidas psr))
			(cond
				(
					(not (equal var var-natribuida))
					(dolist (i (psr-variavel-restricoes psr var))
						(cond
							(
								(and
									(my-member var-natribuida (restricao-variaveis i))
									(not(my-member (cons var-natribuida var) result))
								)
									(setf
										result
										(append
											result
											(list (cons var-natribuida var))
										)
									)
							)
						)
					)
				)
			)
		)
		result
	)
)


; FOWARD CHECKING

; The simpler technique for evaluating the effect of a specific assignment to a variable.
; Initially a variable is instantiated to a value from its domain.
; Then repeatedly at each step, next variable is instantiated to a value that is
; consistent with the previous assignments.
;
; Different than backtracking, while assigning a value to the current variable,
; arc consistency between the current variable and the uninstantiated variables
; are maintained. By this way, current variable cannot take a value that causes
; an empty domain for one of the uninstantiated variables.
; If there is not such a value, then the algorithm backtracks to the point where
; it can start a new branch.

(defun forward-checking(psr var)
	(let
		(
			(inferencias (make-inferencia))
			(testesTotais 0)
			(lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
			(aux nil)
		)
		(dolist (arco lista-arcos)
			(setf aux (multiple-value-list (revise psr (car arco) (cdr arco) inferencias)))
			(setf testesTotais (+ testesTotais (nth 1 aux)))
			(cond ((nth 0 aux)
					(if (equal (length (GetInferenceDomain (car arco) inferencias)) 0)	
						(return-from forward-checking (values nil testesTotais))))))
	(values inferencias testesTotais)
	)
)


; PROCURA RETROCESSO FC MRV

; This search choose as the first variable to be chosen by the one that has the
; smallest field.
; If more than one variable with minimum domain it will be chosen to appear as
; first in the list of unassigned variables.
;
; The function as it is, is a backtracking search using Forward Checking mechanism 
; and MRV (Minimum Remaining Value) Heuristic into the PSR.

(defun procura-retrocesso-fc-mrv (psr)
	(let ((testesTotais 0) (res nil) (res1 nil) (var nil) (inf nil))
		(if (psr-completo-p psr) 
			(return-from procura-retrocesso-fc-mrv (values psr testesTotais)))
		(setf var (min-remaining-values psr))
		(dolist (atr (psr-variavel-dominio psr var))
			(setf res1 (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf testesTotais (+ testesTotais (nth 1 res1)))
			(if (nth 0 res1)
				(progn
				(psr-adiciona-atribuicao! psr var atr)
				(setf res1 (multiple-value-list (forward-checking psr var)))
			
				(setf testesTotais (+ testesTotais (nth 1 res1)))
				(setf inf (nth 0 res1))
				(if inf
					(progn
					(AddInferences psr inf)
					(setf res1 (multiple-value-list (procura-retrocesso-fc-mrv psr)))
					(setf res (nth 0 res1))
					(setf testesTotais (+ testesTotais (nth 1 res1)))
					(cond ((not (equal res nil)) 
						(return-from procura-retrocesso-fc-mrv (values res testesTotais))))
					(AddInferences psr inf)))
				(psr-remove-atribuicao! psr var))))
	(values nil testesTotais)
	)
)


; EXPANDS LIST

; Expands the list in arc iteratively.

(defun ExpandsList (psr lista inferencia)
	(let (
		  (testesTotais 0) 
		  (aux nil) 
		  (inferencias inferencia) 
		  (lista-arcos lista) 
		  (return-arcos nil) 
		  (novos-arcos nil)
		 )
		(dolist (arco lista-arcos)
			(setf aux (multiple-value-list (revise psr (car arco) (cdr arco) inferencias)))
			(setf testesTotais (+ testesTotais (nth 1 aux)))
			(cond ((nth 0 aux)
				(if (equal (length (GetInferenceDomain (car arco) inferencias)) 0)
					(return-from ExpandsList (values nil testesTotais return-arcos inferencia)))
				(setf novos-arcos (arcos-vizinhos-nao-atribuidos psr (car arco)))
				(setf novos-arcos (remove (cons (cdr arco) (car arco)) novos-arcos :test 'equal))
				(setf return-arcos (append return-arcos novos-arcos)))
			)
		)
		(values T testesTotais return-arcos inferencia)
	)
)


; MAC (Maintain Arc Consistency)

; This functions propagates variables restrictions of the psr.

(defun MAC (psr var)
	(let ((testesTotais 0) 
		   (inferencias (make-inferencia)) 
		   (lista-arcos (arcos-vizinhos-nao-atribuidos psr var))
		   (aux NIL) 
		   (repeat NIL)
		 )
		(loop do
			(setf aux (multiple-value-list (ExpandsList psr lista-arcos inferencias)))
			(setf repeat(nth 0 aux))
			(setf testesTotais (+ testesTotais (nth 1 aux)))
			(setf lista-arcos (nth 2 aux))
			(setf inferencias (nth 3 aux))
			(if (not repeat)
				(return-from MAC (values nil testesTotais))
			)
			while(not (null lista-arcos))
		) 
		(values inferencias testesTotais)
	)
)


; PROCURA RETROCESSO MAC MRV

; Search for regression solving the CSP used by MAC (Maintain Arc Consistency)
; function and applying MRV heuristic model.

(defun procura-retrocesso-mac-mrv (psr)
	(let ((testesTotais 0) (res nil) (res1 nil) (var nil) (inf nil))
		(cond ((psr-completo-p psr) 
			(return-from procura-retrocesso-mac-mrv (values psr testesTotais))))
		(setf var (min-remaining-values psr))	
		(dolist (atr (psr-variavel-dominio psr var))
			(setf res1 (multiple-value-list (psr-atribuicao-consistente-p psr var atr)))
			(setf testesTotais (+ testesTotais (nth 1 res1)))			
			(cond ((nth 0 res1)
				(psr-adiciona-atribuicao! psr var atr)
				(setf res1 (multiple-value-list (MAC psr var)))
				(setf testesTotais (+ testesTotais (nth 1 res1)))
				(setf inf (nth 0 res1))
				(cond (inf
					(AddInferences psr inf)
					(setf res1 (multiple-value-list (procura-retrocesso-mac-mrv psr)))
					(setf res (nth 0 res1))
					(setf testesTotais (+ testesTotais (nth 1 res1)))
					(cond ((not (equal res nil)) 
						(return-from procura-retrocesso-mac-mrv (values res testesTotais))))
					(AddInferences psr inf)))
				(psr-remove-atribuicao! psr var))))
	(values nil testesTotais)))


; ======================================================================================= ;
;                                RESOLVE-BEST HEURISTICS                                  ;
; ======================================================================================= ;

; INIT PSR

; Initialize the PSR from a Fill-a-Pix puzzle.

(defun InitPSR (arr)
	(fill-a-pix->psr arr)
)


; EXPLORE PSR CONSTRAINS

; Evaluates if the PSR has duplicate entrys of constrains variables and compares the
; length with the length of the non assigned variables of the PSR.

(defun ExplorePSRconstr (psr)
	(let ( (lstaux (psr-listconstrains psr))
			(listfin NIL)
			)
		(dotimes (i (list-length lstaux))
			(setf listfin (append listfin (restricao-variaveis (nth i lstaux) )) )
		)
	(setf listfin (remove-duplicates listfin))
	(if (> (list-length (psr-variaveis-nao-atribuidas psr)) (list-length listfin))		
	(setf listfin (set-difference (psr-variaveis-nao-atribuidas psr) listfin :test 'equal))
	(setf listfin (set-difference listfin (psr-variaveis-nao-atribuidas psr) :test 'equal))
	)
	listfin
	)
)


; GET NINES AND ZEROS

; Search the nines and zeros values into the neighbours of the Array.

(defun GetNinesAndZeros (arr)
	(let (  (lines (array-dimension arr 0))
		    (columns (array-dimension arr 1))
		    (listNines NIL)
		    (listZeros NIL)
			)
		
		 (dotimes (i lines)
			(dotimes (j columns)
				(progn
					(if (equal (aref arr i j) 9)
						(setf listNines (append listNines  (GetNeighboursOfArrayCell arr i j)) )
					)
					(if (equal (aref arr i j) 0)
						(setf listZeros (append listZeros (GetNeighboursOfArrayCell arr i j)) )
					)
				)
			)
		)
		(cons listNines listZeros)
	)
)


; PRE-PROCESS CORNERS

; Process the Array Corners using the GetNeighboursOfArrayCell function to
; calculate the position of the corner.

(defun PreProcessCorners (arr)
	(let
		(
			(tlc (cons 0 0))
	    (trc (cons 0 (- (array-dimension arr 1) 1)) )
	    (blc (cons (- (array-dimension arr 0) 1) 0) )
	    (brc (cons (- (array-dimension arr 0) 1) (- (array-dimension arr 1) 1)) )
	    (listCorners NIL)
		)
			
		(if (equal (aref arr (car tlc) (cdr tlc) ) 4)
			(setf
				listCorners
				(append
					listCorners
					(GetNeighboursOfArrayCell arr (car tlc) (cdr tlc))
				)
			)
		)
		
		(if (equal (aref arr (car trc) (cdr trc) ) 4)
			(setf
				listCorners
				(append
					listCorners
					(GetNeighboursOfArrayCell arr (car trc) (cdr trc))
				)
			)
		)
		
		(if (equal (aref arr (car blc) (cdr blc) ) 4)
			(setf
				listCorners
				(append
					listCorners
					(GetNeighboursOfArrayCell arr (car blc) (cdr blc))
				)
			)
		)
		
		(if (equal (aref arr (car brc) (cdr brc) ) 4)
			(setf
				listCorners
				(append
					listCorners
					(GetNeighboursOfArrayCell arr (car brc) (cdr brc))
				)
			)
		)
		listCorners				
	)
)


; ADD ATRIBUTIONS TO CORNERS

; Add the respective atributions to the corners of the Array.

(defun AddAtributionsToCorners (psr arr)
	(let ( (listaCorners (PreProcessCorners arr))
		)
		 (dotimes (j (list-length listaCorners))
			(psr-adiciona-atribuicao! psr (nth j listaCorners) 1)
		 )
	NIL			
	)
)


; PRE-PROCESS ZEROS AND NINES

; Pre-Process the Zeros and Nines using the aux functions created for it.

(defun PreProcessZerosAndNines (arr psr)
	(let ( (lista1 (car (GetNinesAndZeros arr)))
	        (lista2 (cdr (GetNinesAndZeros arr)))
	
			)
		(dotimes (i (list-length lista1))
			(psr-adiciona-atribuicao! psr (nth i lista1) 1)
		)
		
		(dotimes (i (list-length lista2))
			(psr-adiciona-atribuicao! psr (nth i lista2) 0)
		)
	NIL		
	)
)

; RESOLVE BEST

; Use the Best Algorithm to solve the received Fill-a-Pix.

(defun resolve-best (arr)
	(let
		(
			(psr1 NIL)
		)
			
		(setf psr1 (InitPSR arr))
		(ExplorePSRconstr psr1)
		(GetNinesAndZeros arr)
		(PreProcessZerosAndNines arr psr1)
		(PreProcessCorners arr)
		(AddAtributionsToCorners psr1 arr)
		(psr->fill-a-pix
			(procura-retrocesso-fc-mrv psr1)
			(array-dimension arr 0)
			(array-dimension arr 1)
		)
	)
)
