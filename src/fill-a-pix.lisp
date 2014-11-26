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

(load "exemplos.lisp")




; ======================================================================================= ;
;                               DEFINICAO DAS ESTRUTURAS DE DADOS                         ;
; ======================================================================================= ;


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
; ======================================================================================= ;
;                                                                                         ;
;                                  PROJECT | FIRST PART                                   ;
;                                                                                         ;
; ======================================================================================= ;
; ======================================================================================= ;




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
    (let ((maxsz (list-length (PSR-listvars psr)))
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
				(dotimes (i maxsz );;(PSR-listvars psr))
					(if (equal (nth i (PSR-listvars psr)) var)
						(setf (nth i (PSR-listvalues psr)) val)
					)
				)
	)
)


; REMOVE ATRIBUICAO

; Este modificador recebe um PSR e uma variavel,
; e altera o PSR recebido, removendo qualquer 
; atribuicao a variavel que tenha sido feita anteriormente.
; A variavel passa efetivamente a nao estar atribuida.
; O valor de retorno desta funcao nao esta definido.


(defun psr-remove-atribuicao! (psr var) 
	(let
		(
			(maxsz (list-length (PSR-listvars psr)))
		)
		(dotimes (i maxsz (PSR-listvars psr))
			(if (equal (nth i (PSR-listvars psr)) var) 
				(setf
					(nth i (PSR-listvalues psr))
					NIL
				)
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

; 1) Valor logico que e T se o PSR for consistente,
;    ou seja, se todas as restricoes do PSR se verificarem
;    para as atribuicoes existentes, e NIL cc.

; 2) Numero de testes de consistencia que foram
;    necessarios para determinar a consistencia.

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

; 1) Valor logico que e T se a variavel e consistente
;    tendo em conta as atribuicoes do PSR.

; 2) Retornado corresponde ao numero de testes de
;    consistencia que foram necessarios para
;    determinar a consistencia.

(defun psr-variavel-consistente-p (psr var)
	(let
		(
			(psr1 (cria-psr
							(PSR-listvars psr)
							(PSR-listdomains psr)
							(copy-list
								(psr-variavel-restricoes psr var)
							)
						)
			)
		(sz (list-length (PSR-listvalues psr))))
		
		(dotimes (i sz)
			(setf (nth i (PSR-listvalues psr1) ) (nth i (PSR-listvalues psr) ) )
		)
		
		(psr-consistente-p psr1)
	)
)




; PSR ATRIBUICAO CONSISTENTE

; Este teste recebe um PSR, uma variavel e um valor,
; e retorna 2 valores.

(defun psr-atribuicao-consistente-p (psr var valor)
	(let
		(
			(psr1 (cria-psr
							(PSR-listvars psr)
							(PSR-listdomains psr)
							(copy-list (psr-variavel-restricoes psr var))
						)
			)
			(result nil)
			(sz (list-length (PSR-listvalues psr)))
		)
		
		(dotimes (i sz)
			(setf
				(nth i (PSR-listvalues psr1))
				(nth i (PSR-listvalues psr))
			)
		)
		
		(psr-adiciona-atribuicao! psr1 var valor)
		
		(setf
			result
			(multiple-value-list
				(psr-variavel-consistente-p psr1 var)
			)
		)
		
		(values
			(car result)
			(car (cdr result))
		)
	)
)




; PSR ATRIBUICOES CONSISTENTES EM ARCO

; Este teste recebe um PSR, uma variavel v1,
; e um valor para essa variavel, uma variavel v2 e um 
; valor para essa variavel, e retorna 2 valores.

(defun psr-atribuicoes-consistentes-arco-p (psr var1 val1 var2 val2)
	(let
		(
			(psr1 (cria-psr
							(PSR-listvars psr)
							(PSR-listdomains psr)
							(intersection
								(psr-variavel-restricoes psr var1)
								(psr-variavel-restricoes psr var2)
							)
						)
			)
			(oldval1 (psr-variavel-valor psr var1))
			(sz (list-length (PSR-listvalues psr)))
			(oldval2 (psr-variavel-valor psr var2))
			(result NIL)
		)
		(dotimes (i sz)
			(setf
				(nth i (PSR-listvalues psr1))
				(nth i (PSR-listvalues psr))
			)
		)

		(psr-adiciona-atribuicao! psr1 var1 val1)
		(psr-adiciona-atribuicao! psr1 var2 val2)
		
		(setf
			result
			(multiple-value-list
				(psr-consistente-p psr1)
			)
		)
		
		(if oldval1 (psr-adiciona-atribuicao! psr var1 oldval1)
			(psr-remove-atribuicao! psr var1)
		)
		(if oldval2 (psr-adiciona-atribuicao! psr var2 oldval2)
			(psr-remove-atribuicao! psr var2)
		)
		
		(values
			(car result)
			(car (cdr result))
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
;    lista de vizinhos;
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

		; Coloca o array aux apenas a -1
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


; TRANSFORMA PSR EM FILL-A-PIX

; Recebe um PSR resolvido, um inteiro que representa o numero de linhas l,
; e outro que representa o numero de colunas c, e devolve um array bidimensional
; de l linhas e c colunas, contendo para cada posicao linha/coluna a atribuicao
; da variavel correspondente do PSR.
; Nesta implementacao apenas foi necessario criar o nome da variavel,
; sendo este o indice do valor da variavel do nosso PSR em string.

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

; Recebe um PSR, e tenta resolve-lo usando uma procura com retrocesso simples,
; i.e. sem usar qualquer heuristica para escolher a proxima variavel e valor,
; e sem qualquer mecanismo de inferencia.
; Retorna 2 valores, o primeiro e o PSR resolvido, ou NIL caso nao exista solucao.

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

; Recebe um array com um puzzle Fill-a-Pix por resolver, tenta resolve-lo usando a
; procura-retrocesso-simples e retorna o resultado final como um array do problema
; Fill-a-Pix resolvido.
; Se nao houver solucao, deve ser retornado NIL.

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
; ReCalculateGlobalDegrees: re-calculate the global degrees of the pair list;

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