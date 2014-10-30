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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ESTRUTURAS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar n-current-task 0)
  
  
; PROBLEMA
; initial-state - root da arvore
; solution - verifica se é estado final
; actions - tarefas a "escolher"
; result - todas as escolhas possiveis
; tree-cost - total de horas ate aquele ponto
 
 (defstruct problem
	initial-state
	solution
	actions
	result
	tree-cost)

	
; INFO-NO
; estrutura que guarda o pai e filhos de um determinado no

(defstruct no
	state
	task
	father	
	h
	g)

	
; SATISFACAO DE RESTRICOES

(defstruct csp 
	variaveis 
	dominios 
	restricoes)