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



; ======================================================================================= ;
;                                             NOTAS                                       ;
; ======================================================================================= ;

;; Test using "clisp -i tipos_de_dados.lisp -i exemplos.lisp < test01/input" by RZL




; ======================================================================================= ;
;                                             LOAD                                        ;
; ======================================================================================= ;

; Invocacao dos ficheiros de teste.

;;;; =====> UNCOMMENT AS YOU NEED <===== ;;;;


;; (load "exemplos.fas")

;; (load "exemplos.lisp")




; ======================================================================================= ;
;                               DEFINICAO DAS ESTRUTURAS DE DADOS                         ;
; ======================================================================================= ;


(defun check-CSP (p lst)
    (cond 
    (   (null lst)           NIL   )
    (   (funcall p (first lst))  T )
    (  T (check-CSP p  (rest lst)) )
    )
)


(defstruct restricao 
    (listvars NIL)
    (pred NIL)
)


; HASHTABLE GLOBAL

; As variaveis do nosso PSR irao servir para indexar a Hashtable
; e desta forma acedemos aos respectivos valores com um custo menor.

(defparameter myhashtable (make-hash-table :test 'equal))
(defparameter hashtable2 (make-hash-table :test 'equal))




; ======================================================================================= ;
;                                        TIPO RESTRICAO                                   ;
; ======================================================================================= ;

; O tipo Restricao tem como argumentos uma lista de variaveis e um predicado que avalia
; se uma dada CSP e valida ou nao.
; O predicado ira ter que chamar a funcao check-CSP para que assim construa a restricao.


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
    (loop for val being each hash-value of table always (= val 0))
	)
)

(defstruct PSR
    (listvars NIL)
    (listdomains NIL)
    (listconstrains NIL)
    
    (listvalues NIL)
)




; ======================================================================================= ;
;                                              PSR                                        ;
; ======================================================================================= ;

; O tipo Problema de Satisfacao de Restricoes (PSR) e utilizado para representar
; um problema de satisfacao de restricoes, que guarda informacao acerca das variaveis,
; dominios e restricoes de um Problema de Satisfacao de Restricoes (PSR).


; LISTAS A USAR

; listvars - lista de variaveis;
; listdomains - lista de dominios;
; listconstrains - lista de restricoes;
; listvalues - lista de atribuicoes.


; CRIA PSR

; Este construtor recebe um lista de variaeis, uma lista de dominios
; com o mesmo tamanho da lista de variaveis, e uma lista de restricoes.
; Associa variaveis e dominios numa Hashtable onde facilmente se constroi
; as estruturas e retornamos NIL.

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


; ATRIBUICOES DO PSR

; Recebe um PSR, e retorna uma lista com todas as atribuicoes do PSR.
; Nesta funcao utilizamos uma funcao aux para verificar se construimos
; uma lista de pares.

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
	
	
	)
)


; VARIAVEIS PSR

; Recebe um PSR e retorna uma lista com todas as variaveis do PSR, 
; independentemente de estarem ou nao atribuidas.

(defun psr-variaveis-todas (psr)
	(PSR-listvars psr)
)


; VARIAVEIS NAO ATRIBUIDAS

; Recebe um PSR e retorna uma lista com as variaveis do PSR que
; ainda nao foram atribuidas.

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

; Selector recebe um PSR e uma variavel.
; Se a variavel tiver um valor atribuido, e retornado o valor correspondente.
; Se a variavel nao estiver atribuida, e retornado NIL.

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


; DOMINIO DA VARIAVEL

; Retorna o dominio correspondente a essa variavel.

(defun psr-variavel-dominio (psr key)
    (defparameter *myhashtable* (make-hash-table :test 'equal))
    (let ( (maxsz (list-length (PSR-listvars psr)))
           (answ2 NIL)
     			)
				(dotimes (i maxsz (PSR-listvars psr))
	    		(if (equal (nth i (PSR-listvars psr)) key)
						(setf answ2 (nth i (PSR-listdomains psr)))
	     		)
    		)
    		answ2
    )
)


; RESTRICOES DA VARIAVEL

; Retorna uma lista com todas as restricoes aplicaveis a essa variavel.

(defun psr-variavel-restricoes (psr key)
    (defparameter *myhashtable* (make-hash-table :test 'equal))
    (let ((maxsz (list-length (PSR-listconstrains psr)))
          (answ3 NIL) 
     			)
	
	(setf answ3 NIL)
       
       (dotimes (i maxsz (PSR-listconstrains psr))
            (if (not
            			(equal
            				(member key
            								(restricao-listvars
            									(nth i (PSR-listconstrains psr)))
            									:test 'equal)
            				NIL
            			)
            		)
							(setf answ3 (append answ3 (list (nth i (PSR-listconstrains psr)))))
            
            )
            
            )
	    answ3
    )
)


; ADICIONA ATRIBUICAO

; Este modificador recebe um PSR, uma variavel e um valor,
; e altera o PSR recebido, atribuindo o valor a variavel.
; Se a variavel ja tinha sido atribuida, o novo valor substitui o valor anterior.
; O valor de retorno desta funcao nao esta definido.


(defun psr-adiciona-atribuicao! (psr var val)
	(let ((maxsz (list-length (PSR-listvars psr))) )
				(setf (gethash var myhashtable)
							val)
				(dotimes (i maxsz )
					(if (equal (nth i (PSR-listvars psr)) var)
						(setf (nth i (PSR-listvalues psr)) val)
					)
				)
	)
)


; REMOVE ATRIBUICAO

; Este modificador recebe um PSR e uma variAvel,
; e altera o PSR recebido, removendo qualquer 
; atribuicao a variavel que tenha sido feita anteriormente.
; A variavel passa efetivamente a nao estar atribuída.
; O valor de retorno desta funcao nao esta definido.


(defun psr-remove-atribuicao! (psr var) 
	(let ((maxsz (list-length (PSR-listvars psr))) )

				(dotimes (i maxsz (PSR-listvars psr))
					(if (equal (nth i (PSR-listvars psr)) var) 
						(setf (nth i (PSR-listvalues psr)) NIL)
					)
				)
	)
)


; ALTERA DOMINIO

; Este modificador recebe um PSR, uma variavel,
; e um dominio e altera o PSR recebido,
; alterando o dominio associado a variavel recebida,
; para que passe a ser o dominio recebido.
; O valor de retorno desta funcao nao esta definido.


(defun psr-altera-dominio! (psr var dom)
	(let ((maxsz (list-length (PSR-listvars psr))) )
		(dotimes (i maxsz (PSR-listvars psr))
			(if (equal (nth i (PSR-listvars psr)) var) 
				(setf (nth i (PSR-listdomains psr)) dom)
			)
		)
	)
)


; PSR COMPLETO

; Recebe um PSR e retorna T se estiver completo,
; i.e. se tiver uma atribuicao para todas as variaveis.
; Retorna NIL caso contrario.

(defun psr-completo-p (psr)
	(let ((maxsz (list-length (PSR-listvars psr)))
	       (auxLi NIL)
	       (resultado 0) )
	     
		(setq auxLi (listOfPairsBuild (PSR-listvars psr) (PSR-listvalues psr)))
		
		(dotimes (i maxsz (PSR-listvars psr))
					(if (not (equal (cdr (nth i auxLi)) NIL) )
						(incf resultado)
				)
		)
		(cond
		     ((equal (list-length auxLi) resultado ) T)
		       (T NIL)
		)
	)
)


; PSR CONSISTENTE

; 1) Valor logico que e T se o PSR for consistente,
;    ou seja, se todas as restricoes do PSR se verificarem
;    para as atribuicoes existentes, e NIL cc.

; 2) Numero de testes de consistencia que foram
;    necessarios para determinar a consistencia.

(defun psr-consistente-p (psr)

	(block alpha
    (let ((maxsz (list-length (PSR-listconstrains psr)))
	    		(cont 0)
	    		(ans T)
      		(flag T))
   			(defvar fun)
	    	(dotimes (i maxsz )
			    (incf cont)
				    (setq fun (restricao-pred (nth i (PSR-listconstrains psr))) )
				    (setf ans (funcall fun psr))
				    (if (equal ans NIL)
							(return-from alpha (values-list (list NIL cont)))
					  )
				)
				(return-from alpha (values-list (list flag cont)))
    )
	)
)



; PSR VARIAVEL CONSISTENTE

; 1) Valor logico que e T se a variavel e consistente
;    tendo em conta as atribuicoes do PSR.

; 2) Retornado corresponde ao numero de testes de
;    consistencia que foram necessarios para
;    determinar a consistencia.

(defun psr-variavel-consistente-p (psr var)
    (block alpha
    (let ((maxsz (list-length (psr-variavel-restricoes psr var)))
	    (cont 0)
	    (ans T)
	      (flag T))
	     (defvar fun)
	    (dotimes (i maxsz )
		    (incf cont)
			    (setq fun (restricao-pred (nth i (psr-variavel-restricoes psr var))) )
			    
			    
			    (setf ans (funcall fun psr))
			    (if (equal ans NIL)
						(return-from alpha (values-list (list NIL cont)))
					
					
			    
				    )
			    )
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
	    (dotimes (i maxsz )
		    (incf cont)
			    (setq fun (restricao-pred (nth i (psr-variavel-restricoes psr var))) )
			    
			    
			    (setf ans (funcall fun psr))
			    (if (equal ans NIL)
						(return-from alpha cont)
					
					
			    
				    )
		    
			    
	    )
	 (return-from alpha cont)		
    )
   
    )

)




; PSR ATRIBUICAO CONSISTENTE

; Este teste recebe um PSR, uma variável e um valor,
; e retorna 2 valores.

(defun psr-atribuicao-consistente-p (psr var val)
	(let (
		 (flag T)
		 (oldval (psr-variavel-valor psr var))
	(psr-adiciona-atribuicao! psr var val)	
	(if (equal (psr-variavel-consistente-p psr var) NIL)
			(setf flag NIL)
	)
		(psr-adiciona-atribuicao! psr var oldval)	
	(values flag (psr-variavel-consistente-pVALOR psr var))
))

(defun psr-consistente-pAuxiliar (psr auxList)

	(block alpha
    (let ((maxsz (list-length auxList))
	    (cont 0)
	    (ans T)
	      (flag T))
	     (defvar fun)
	    (dotimes (i maxsz )
		    (incf cont)
			    (setq fun (restricao-pred (nth i auxList)) )
			    (setf ans (funcall fun psr))
			    
			   
			    
			    (if (equal ans NIL)
						(return-from alpha (values-list (list NIL)))
					
					
			    
				    )	    
	    )
	   (return-from alpha (values-list (list flag)))
			
    )
))

(defun psr-consistente-pAuxiliarValues (psr auxList)

	(block alpha
    (let ((maxsz (list-length auxList))
	    (cont 0)
	    (ans T))
	     (defvar fun)
	    (dotimes (i maxsz )
		    (incf cont)
			    (setq fun (restricao-pred (nth i auxList)) )			    
			    (setf ans (funcall fun psr))
			    (if (equal ans NIL)
						(return-from alpha (values-list (list cont)))			    
				    )			    
	    )    
	   (return-from alpha (values-list (list cont)))
    )
))


; PSR ATRIBUICOES CONSISTENTES EM ARCO

; Este teste recebe um PSR, uma variavel v1,
; e um valor para essa variavel, uma variavel v2 e um 
; valor para essa variavel, e retorna 2 valores.

(defun psr-atribuicoes-consistentes-arco-p (psr var1 val1 var2 val2)
    (let (
	     (oldval1 (psr-variavel-valor psr var1)) 
	     (oldval2 (psr-variavel-valor psr var2))
	     (ansDebug NIL)
	     (novopsr NIL)
		)
	
		(setf novopsr
					(cria-psr (PSR-listvars psr)
										(PSR-listdomains psr)
										(PSR-listconstrains psr)
					)
		)
		(psr-adiciona-atribuicao! psr var1 val1)
		(psr-adiciona-atribuicao! psr var2 val2)
		(psr-adiciona-atribuicao! novopsr var1 val1)
		(psr-adiciona-atribuicao! novopsr var2 val2)	
		(setq ansDebug
					(intersection (psr-variavel-restricoes psr var1)
			    							(psr-variavel-restricoes psr var2)
			    )
		)
		(psr-adiciona-atribuicao! psr var1 oldval1) 
		(psr-adiciona-atribuicao! psr var2 oldval2)
		(values
			(psr-consistente-pAuxiliar novopsr ansDebug)
			(psr-consistente-pAuxiliarValues novopsr ansDebug)
		)
	)
)




; ======================================================================================= ;
;                                    FUNCOES DE CONVERSAO                                 ;
; ======================================================================================= ;


; TRANSFORMA FILL-A-PIX EM PSR

; Recebe um array correspondente a uma tabela bidimensional m x n de um puzzle
; Fill-a-Pix por resolver e retorna um PSR que representa o problema de resolver
; esse puzzle particular.
;
; 1) Nesta funcao verifica-se e coloca-se todos os vizinhos de uma variavel numa
; lista de vizinhos;
; 
; 2) Cria um predicado que recebe um Array e os respectivos indices do mesmo, para
;    calculo do tamanho dele proprio;
;
; 3) Atribui o nome das variaveis igual aos indices e coloca numa lista de strings
;    para ser avaliado como um nome;
;
; 4) Coloca todas as restricoes verificadas numa lista de restricoes,
;    em que cada restricao e aplicada segundo um dado predicado verificando
;    todos os seus vizinhos em redor;
;
; 5) Atribui o dominio 0 ou 1 (branco ou preto) numa lista de dominios
;    as variaveis correspondentes;
;
; Por fim cria o PSR com os argumentos  das novas lista de variaveis,
; lista de dominios e lista de restricoes.

(defun GetNeighboursOfArrayCell (arr i j)
	(let ((listaVizinhos NIL))

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
			(setf listaVizinhos (append listaVizinhos (list (gethash (cons (+ i 1) (+ j 1)) hashtable2)))))
		)
		listaVizinhos	
	)
)



(defun CriaPredicado (arr i j)

	(let(
		(listaviz (GetNeighboursOfArrayCell arr i j))
	       (valor (aref arr i j)) )  

	(setq fun #'(lambda (psr)
	(let ( 
	
	       (nones 0)
	       (nzeros 0)
	       (soma 0)
	       (sumaux 0 ) )
	       
	     (dotimes (i (list-length listaviz) )
				
			
	     
				(if (equal (psr-variavel-valor psr (nth i listaviz)) 1) 
					(incf nones)
				)
				
				(if (equal (psr-variavel-valor psr (nth i listaviz)) 0) 
					(incf nzeros)
				)
	     )
	    
	
	     
	     (dotimes (i (list-length listaviz) )
			(if (not (equal NIL (psr-variavel-valor psr (nth i listaviz) )) )
				(setf soma (+ soma (psr-variavel-valor psr (nth i listaviz) )))
		    )
	     )
	 (setf sumaux nones)
	 (if (and (<= sumaux valor) ( >= (- (list-length listaviz) valor) nzeros))
	 				T
	 			)
	 )
	))  
	 
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
		 (defparameter aux (make-array (list lines columns )))
		 (dotimes (i lines)
			(dotimes (j columns)
				(setf (aref aux i j) (aref arr i j))
				(setf (gethash (cons i j) hashtable2) (write-to-string cont))
				(setf listadevars (append listadevars (list (write-to-string cont))))
				(incf cont)
			)
		)
		(dotimes (i lines)
			(dotimes (j columns)
				(if (not (equal NIL (aref arr i j) ))
					(setf listaderestr
						(append
							listaderestr
							(list
								(cria-restricao (GetNeighboursOfArrayCell arr i j)
																(CriaPredicado aux i j)
								)
							)
						)
					)
				)	
			)
		)	
		(setf listadedoms (make-list cont :initial-element '(0 1)))
		(cria-psr listadevars listadedoms listaderestr)
		)
)


; TRANSFORMA PSR EM FILL-A-PIX

; Recebe um PSR resolvido, um inteiro que representa o numero de linhas l,
; e outro que representa o numero de colunas c, e devolve um array bidimensional
; de l linhas e c colunas, contendo para cada posicao linha/coluna a atribuição
; da variavel correspondente do PSR.
; Nesta implementacao apenas foi necessario criar o nome da variavel,
; sendo este o indice do valor da variavel do nosso PSR em string.

(defun psr->fill-a-pix (psr lin col)
	(defparameter aux (make-array (list lin col )))
	(dotimes (i lin)
			(dotimes (j col)
				(setf (aref aux i j) (psr-variavel-valor psr (write-to-string i)) )
			)
	)
	aux
)




; ======================================================================================= ;
;                               PROCURA RETROCESSO SIMPLES                                ;
; ======================================================================================= ;


; PROCURA RETROCESSO SIMPLES

; Recebe um PSR, e tenta resolve-lo usando uma procura com retrocesso simples,
; i.e. sem usar qualquer heuristica para escolher a proxima variavel e valor,
; e sem qualquer mecanismo de inferencia.
; Retorna 2 valores, o primeiro e o PSR resolvido, ou NIL caso nao exista solucao.

(defvar final 0)

(defun procura-retrocesso-simples (psr)
	(let (
			(var (car (psr-variaveis-nao-atribuidas psr)))
			(resultado T)
			(consistency_checks 0)
			(isvalid NIL)
			)
			(dolist (val (psr-variavel-dominio psr var))
				(if (equal (psr-completo-p psr) NIL)
					(progn
						(multiple-value-bind (resultado test)
							(psr-atribuicao-consistente-p psr var val) 
							(setf isvalid resultado)
							(setf consistency_checks
								(+ consistency_checks test)))
							(if isvalid
							(progn
								(psr-adiciona-atribuicao! psr var val)
								(multiple-value-bind (ret testes-feitos)
									(procura-retrocesso-simples psr) 
									(setf resultado ret)
									(setf consistency_checks
										(+ consistency_checks testes-feitos)))
								(if (eq nil resultado)
									(progn
										(psr-remove-atribuicao! psr var))
									))))))
			(if resultado
				(values psr consistency_checks)
				(values nil consistency_checks)
			)
	)
)


; RESOLVE SIMPLES

; Recebe um array com um puzzle Fill-a-Pix por resolver, tenta resolve-lo usando a
; procura-retrocesso-simples e retorna o resultado final como um array do problema
; Fill-a-Pix resolvido.
; Se nao houver solucao, deve ser retornado NIL.

(defun resolve-simples (arr) 
	(psr->fill-a-pix (procura-retrocesso-simples
											(fill-a-pix->psr arr)
										)
										(array-dimension arr 0)
										(array-dimension arr 1)
	)
)