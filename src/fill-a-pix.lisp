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




; ======================================================================================= ;
;                                        TIPO RESTRICAO                                   ;
; ======================================================================================= ;

; O tipo Restricao tem como argumentos uma lista de variaveis e um predicado que avalia
; se uma dada CSP e valida ou nao.
; O predicado ira ter que chamar a funcao check-CSP para que assim construa a restricao.


(defun cria-restricao (lstvars predi)
	(
    setf x (make-restricao :listvars lstvars :pred predi)
  )
  x
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

; lstv - lista de variaveis;
; lstd - lista de dominios;
; lstr - lista de restricoes;
; lsta - lista de atribuicoes.


; CRIA PSR

; Este construtor recebe um lista de variaeis, uma lista de dominios
; com o mesmo tamanho da lista de variaveis, e uma lista de restricoes.
; Associa variaveis e dominios numa Hashtable onde facilmente se constroi
; as estruturas e retornamos NIL.

(defun cria-psr (varsl domainsl constrainsl)
	(let ((maxsz (list-length varsl)) 
				(myhashtable (make-hash-table))
				)

			(dotimes (i maxsz varsl)
				(setf (gethash (nth i varsl) myhashtable) NIL))
	)
	(setf x (make-PSR
						:listvars varsl
						:listdomains domainsl
						:listconstrains constrainsl
						:listvalues NIL))
	x
)


; ATRIBUICOES DO PSR

; Recebe um PSR, e retorna uma lista com todas as atribuicoes do PSR.

(defun psr-atribuicoes (psr)
	(let ((maxsz (list-length (PSR-listvars psr))) 
		  	(myhashtable (make-hash-table :test 'equal))
					(answ ())
				)

				(dotimes (i maxsz (PSR-listvars psr))
					(setf answ
						(append answ (list (cons (nth i (PSR-listvars psr))
																		 (list (nth i (PSR-listvalues psr)))
																)
													)
						)
					)
				(PSR-listvalues psr))
	)
	answ
)


; VARIAVEIS PSR

; Recebe um PSR e retorna uma lista com todas as variaveis do PSR, 
; independentemente de estarem ou nao atribuidas.

(defun psr-variaveis-todas (psr)

	)


; VARIAVEIS NAO ATRIBUIDAS

; Recebe um PSR e retorna uma lista com as variaveis do PSR que
; ainda nao foram atribuidas.

(defun psr-variaveis-nao-atribuidas (psr)

	)


; VALOR DA VARIAVEL

; Selector recebe um PSR e uma variavel.
; Se a variavel tiver um valor atribuido, e retornado o valor correspondente.
; Se a variavel nao estiver atribuida, e retornado NIL.

(defun psr-variavel-valor (psr var)
(let ( (maxsz (list-length (PSR-listvars psr))) 
       (myhashtable (make-hash-table :test 'equal))
     )
       
       (dotimes (i maxsz (PSR-listvars psr))
            (setf (gethash (nth i (PSR-listvars psr)) myhashtable)
            			(nth i (PSR-listvalues psr)))
            ;;(print (nth i (PSR-listvalues psr)))
            )
     (gethash var myhashtable)       
    )  
)


; DOMINIO DA VARIAVEL

; Retorna o dominio correspondente a essa variavel.

(defun psr-variavel-dominio (psr key)
    (defparameter *myhashtable* (make-hash-table :test 'equal))
    (let ( (maxsz (list-length (PSR-listvars psr))) 
     )
       
       (dotimes (i maxsz (PSR-listvars psr))
            (setf (gethash (nth i (PSR-listvars psr)) *myhashtable*)
            			(nth i (PSR-listdomains psr)))
            ;;(print (nth i (PSR-listvars psr)))
            ;;(print (gethash (nth i (PSR-listvars psr)) *myhashtable*))
            
            )
    )
    (gethash key *myhashtable*)
)


; RESTRICOES DA VARIAVEL

; Retorna uma lista com todas as restricoes aplicaveis a essa variavel.

(defun psr-variavel-restricao (psr key)
    (defparameter *myhashtable* (make-hash-table :test 'equal))
    (let ((maxsz (list-length (PSR-listconstrains psr))) 
     			)
       
       (dotimes (i maxsz (PSR-listconstrains psr))
            (setf (gethash (nth i (PSR-listconstrains psr)) *myhashtable*)
            			(nth i (PSR-listconstrains psr)))
            
            )
    )
    (gethash key *myhashtable*)
)




; ======================================================================================= ;
;											/ APAGAR \
;
; NOTA: tentar compreender como funcionam as funcoes sem retorno...
;
; Talvez nao seja para utilizar estas funcoes para ja.
;
; ======================================================================================= ;


; ADICIONA ATRIBUICAO

; Este modificador recebe um PSR, uma variavel e um valor,
; e altera o PSR recebido, atribuindo o valor a variavel.
; Se a variavel ja tinha sido atribuida, o novo valor substitui o valor anterior.
; O valor de retorno desta funcao nao esta definido.

(defun psr-adiciona-atribuicao! (psr var val)
	(defparameter *myhashtable* (make-hash-table :test 'equal))
    (let ( (maxsz (list-length (PSR-listvars psr))) )
       (dotimes (i maxsz (PSR-listvars psr))
          (if ((setq val (gethash var myhashtable)))
          	(puthash var val myHash)
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

	)


; ALTERA DOMINIO

; Este modificador recebe um PSR, uma variavel,
; e um dominio e altera o PSR recebido,
; alterando o dominio associado a variavel recebida,
; para que passe a ser o dominio recebido.
; O valor de retorno desta funcao nao esta definido.


(defun psr-altera-dominio! (psr var)

	)


; PSR COMPLETO

; Recebe um PSR e retorna T se estiver completo,
; i.e. se tiver uma atribuicao para todas as variaveis.
; Retorna NIL caso contrario.

(defun psr-completo-p (psr)

	)


; PSR CONSISTENTE

; 1) Valor logico que e T se o PSR for consistente,
;    ou seja, se todas as restricoes do PSR se verificarem
;    para as atribuicoes existentes, e NIL cc.

; 2) Numero de testes de consistencia que foram
;    necessarios para determinar a consistencia.


(defun psr-consistente-p (psr)

	)


; PSR VARIAVEL CONSISTENTE

; 1) Valor logico que e T se a variavel e consistente
;    tendo em conta as atribuicoes do PSR.

; 2) Retornado corresponde ao numero de testes de
;    consistencia que foram necessarios para
;    determinar a consistencia.

(defun psr-variavel-consistente-p (psr var)

	)




; ======================================================================================= ;
;											/ APAGAR \
;
; RETORNO: logico, inteiro
;
; logico: T se PSR for consistente, ou seja, se todas as restricoes se verificarem
;         para atribuicoes existentes.
;
; inteiro: n dos testes de consistencia.
;
; ======================================================================================= ;


; PSR ATRIBUICAO CONSISTENTE

; Este teste recebe um PSR, uma variável e um valor,
; e retorna 2 valores.

(defun psr-atribuicao-consistente-p (psr var val)

	)


; PSR ATRIBUICOES CONSISTENTES EM ARCO

; Este teste recebe um PSR, uma variavel v1,
; e um valor para essa variavel, uma variavel v2 e um 
; valor para essa variavel, e retorna 2 valores.

(defun psr-atribuicoes-consistentes-arco-p (psr var1 val1 var2 val2)

	)

