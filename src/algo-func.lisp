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
;                                   ALGORITMOS PARA FUNCOES                                ;
; ======================================================================================== ;

; Funcoes dos algoritmos a implementar para o jogo Fill-a-Pix.


; LOAD

; Chamada dos Ficheiros

(load "aux-func.lisp")


; INSERE LISTA EM ORDEM NOUTRA LISTA - NOT GREEDY

; Coloca uma lista na outra, com os elementos ordenados.

(defun list-insert-order (successors successors_generated)
	(dolist (gen successors_generated)
		(setf successors(list-insert-elem gen successors))
	)
	(return-from list-insert-order successors)
)


; COLOCA ELEMENTO POR ORDEM - GREEDY

; Coloca um elemento na lst por ordem.

(defun list-insert-elem-greedy (el lst)
	(let ((n 1))
		(dolist (e lst)
			(if (< (no-h el) (no-h e))
				(progn 
					(setf lst (list-nth-insert el lst n))
					(return-from list-insert-elem-greedy lst)
				)
				(setf n (+ 1 n)))
		)
		(setf lst (list-nth-insert el lst n))
		(return-from list-insert-elem-greedy lst)
	)
)		


; COLOCA UMA DADA LISTA POR ORDEM - GREEDY

; Coloca uma lst na outra, com os elementos ordenados.

(defun list-insert-order-greedy (successors successors_generated)
	(dolist (gen successors_generated)
		(setf successors(list-insert-elem-greedy gen successors))
	)
	(return-from list-insert-order-greedy successors)
)
 
 
; COLOCA ELEMENTO POR ORDEM - A*

; Coloca um elemento na lst por ordem.

(defun list-insert-elem-A (el lst)
	(let ((n 1))
		(dolist (e lst)
			(if (< (+ (in-g el) (in-h el)) 
					(+ (in-g e) (in-h e)))
				(progn 
					(setf lst (list-nth-insert el lst n))
					(return-from list-insert-elem-A lst)
				)
				(setf n (+ 1 n)))
		)
		(setf lst (list-nth-insert el lst n))
		(return-from list-insert-elem-A lst)
	)
)		


; COLOCA LISTA POR ORDEM - A*

; Coloca uma lst na outra, com os elementos ordenados.

(defun list-insert-order-A (successors successors_generated)
	(dolist (gen successors_generated)
		(setf successors(list-insert-elem-A gen successors))
	)
	(return-from list-insert-order-A successors)
)


; VERIFICA SE E SOLUCAO

; Verifica se o state recebido e solucao final do problema.

(defun checks-sol1 (state)
	(let ((x (in-tasks state)) (state-current (in-state state)) (ntask 0))
		(dolist (alt state-current)
			(dolist (task x)
				(if (belongs-a-lst alt tar)
					(setq x (list-nth-delete (+ 1 ntask) x)) 
					(setq ntask (+ 1 ntask))
				)
			)
			(setq ntask 0)
		)
		(setf (in-tasks state) x)
		(if (not x) (return-from checks-sol1 T) (return-from checks-sol1 NIL))
	)
) 


; VERIFICA SE E SOLUCAO - CPS

; Verifica se uma dada lista e solucao para o problema de satisfacao de restricoes.

(defun checks-if-sol (lst)
	(dolist (alt lst)
		(if (equal 'T alt) (return-from checks-if-sol NIL))
	)
	(return-from checks-if-sol T)
)