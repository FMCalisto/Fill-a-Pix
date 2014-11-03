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
;                                         Tipo Restricao                                   ;
; ======================================================================================== ;

; O tipo Restricao e utilizado para representar uma restricao de um PSR.


; LOAD

; Chamada dos Ficheiros

(load "aux-func.lisp")
(load "structures.lisp")


; CRIA RESTRICAO

; Este construtor recebe uma lista das variaveis envolvidas na restricao,
; e uma funcao que verifica a restricao.

(defun cria-restricao (lst pred)

	)


; RESTRICAO VARIAVEIS

; Este selector, dada uma restricao r,
; retorna uma lista com todos as variaveis envolvidas na restricao.

(defun restricao-variaveis (r)

	)


; RESTRICAO FUNCAO VALIDACAO

; Este selector, dada uma restricao r,
; retorna a funcao de avaliacao usada para verificar se a restricao e satisfeita.

(defun restricao-funcao-validacao (r)

	)