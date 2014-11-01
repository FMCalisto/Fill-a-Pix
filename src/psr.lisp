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


; ======================================================================================== ;
;                                              PSR                                         ;
; ======================================================================================== ;

; O tipo Problema de Satisfacao de Restricoes (PSR) e utilizado para representar
; um problema de satisfacao de restricoes, que guarda informacao acerca das variaveis,
; dominios e restricoes de um Problema de Satisfacao de Restricoes (PSR).


; LOAD

; Chamada dos Ficheiros

(load "aux-func.lisp")
(load "setructures.lisp")
(load "restricao.lisp")


; LISTAS A USAR

; lstv - lista de variaveis;
; lstd - lista de dominios;
; lstr - lista de restricoes;
; lsta - lista de atribuicoes.


; CRIA PSR

; Este construtor recebe um lista de variaeis, uma lista de dominios
; com o mesmo tamanho da lista de variaveis, e uma lista de restricoes.

(defun cria-psr (lstv lstd lstr)

	)


; ATRIBUICOES DO PSR

; Recebe um PSR, e retorna uma lista com todas as atribuicoes do PSR.

(defun psr-atribuicoes (psr)

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

	)


; DOMINIO DA VARIAVEL

; Retorna o dominio correspondente a essa variavel.

(defun psr-variavel-dominio (psr var)

	)


; RESTRICOES DA VARIAVEL

; Retorna uma lista com todas as restricoes aplicaveis a essa variavel.

(defun psr-variavel-restricoes (psr var)

	)


; ======================================================================================== ;
;											/ APAGAR \
;
; NOTA: tentar compreender como funcionam as funcoes sem retorno...
;
; Talvez nao seja para utilizar estas funcoes para ja.
; ======================================================================================== ;


; ADICIONA ATRIBUICAO

; Este modificador recebe um PSR, uma variavel e um valor,
; e altera o PSR recebido, atribuindo o valor a variavel.
; Se a variavel ja tinha sido atribuida, o novo valor substitui o valor anterior.
; O valor de retorno desta funcao nao esta definido.

(defun psr-adiciona-atribuicao! (psr var)

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


; ======================================================================================== ;
;											/ APAGAR \
;
; RETORNO: logico, inteiro
;
; logico: T se PSR for consistente, ou seja, se todas as restricoes se verificarem
;         para atribuicoes existentes.
;
; inteiro: n dos testes de consistencia.
;
; ======================================================================================== ;


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

