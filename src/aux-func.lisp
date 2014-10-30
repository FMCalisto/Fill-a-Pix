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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;FUNCOES AUXILIARES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		

; list-nth-delete
; elimina item da posição nth da lista
; o primeiro elemento da lista é 1

(defun list-nth-delete(n list)
  (append (subseq list 0 (1- n)) (nthcdr n list)))
  

; list-nth-insert
; Função que recebe um elemento, uma lista e uma posição 
; e retorna uma nova lista que contém o elemento na      
; desejada. 
; o primeiro elemento da lista é 1

(defun list-nth-insert(elem lst pos)
    (if (or (eql pos 1) (eql lst nil))
            (cons elem lst)
            (cons (car lst) (list-nth-insert elem (cdr lst) (- pos 1)))))

            
; list-last-delete
; retorna a mesma lista, mas sem o ultimo elemento

(defun list-last-delete(lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) nil) 
        (t (cons (car lst) (list-last-delete (cdr lst)))))) 


; list-elem-in
; retorna true se x pertencer a lista l

(defun list-elem-in(elem lst)
  (cond ((null lst) NIL)
        (t (or (equal el (car lista)) (pertence-a-lista el (cdr lista)))))

)


; list-equal
; recebe duas listas (subtarefas) e verificas se são iguais

(defun list-equal (lst1 lst2)
	(let ((answer T))
		(if (equal (list-length lst1) (list-length lst2))
			(if (null lst1) (setf answer NIL) (if (null lst2) (setf answer NIL)
				(cond ((and (= (nth 0 lst1) (nth 0 lst2)) (= (nth 1 lst1) (nth 1 lst2)) (eql (nth 2 lst1) (nth 2 lst2))) (setf answer T)) 
					   (t (setf answer NIL)))
				))
			(setf answer NIL)
		)
		(return-from list-equal answer)
	)
)


; equal-alt
; retorna true se alt1 e alt2 forem iguais

(defun equal-alt (alt1 alt2)
	(let ((list-equal T) (indice 0))
		(loop	
			(if (equal (list-length alt1) (list-length alt2))
				(progn 						
					(if (list-equal (nth indice alt1) (nth indice alt2))
						(progn 
							(setf list-equal T) 
							(setf indice (+ 1 indice)) 
							(if (>= indice (- (list-length alt1) 1)) 
								(return-from equal-alt T)))
						(progn 
							(setf list-equal NIL)
							(return-from equal-alt NIL)
						)
					)
				)
				(progn 
					(setf list-equal NIL) 
					(return)
				) 
			)
		)
		(return-from equal-alt list-equal)
	)
)

;ESTADOS IGUAIS
;retorna true se os dois estados recebidos como argumentos sao iguais
(defun estados-iguais (est1 est2)
	(let ((lista-aux ()) (indice 0))
		(if (equal (list-length est1) (list-length est2))
			(progn
				(setf lista-aux (make-list (list-length est1) :initial-element 'NIL))
				(dolist (alt1 est1)
					(dolist (alt2 est2)
						(if (equal-alt alt1 alt2)
							(progn
								(setf lista-aux (elimina-nth-lista (+ 1 indice) lista-aux))
								(setf lista-aux (insere-nth-lista 'T lista-aux (+ 1 indice)))
							)
						)
					)
					(setf indice (+ 1 indice))
				)
			)
			(return-from estados-iguais NIL)
		)
		(dolist (aux lista-aux)
			(if (equal aux 'NIL)
				(return-from estados-iguais NIL)
			)
		)
		(return-from estados-iguais T)
	)
)

;ESTA-NA-LISTA
;recebe um estado e retorna true se o estado estiver na lista-estados
(defun esta-na-lista (estado lista-estados)
	(let ((pertence T) (estado-es (no-estado estado)))
			(dolist (no-lst lista-estados)
				(if (and estado-es no-lst)
					(progn
						(if (estados-iguais estado-es (no-estado no-lst))
							(progn (setf pertence T) (return-from esta-na-lista pertence))
							(setf pertence NIL)) 
					)
				)
			)
		(return-from esta-na-lista pertence)
	)
)

;VERIFICA
;recebe duas subtarefas e verifica se sao validas juntas
(defun verifica (lst1 lst2)
	(cond ((and (= (nth 0 lst1) (nth 0 lst2)) (/= (nth 1 lst1) (nth 1 lst2)) (not (eql (nth 2 lst1) (nth 2 lst2)))) T) 	;mm dia, hora dif, id dif
		  ((and (/= (nth 0 lst1) (nth 0 lst2)) (/= (nth 1 lst1) (nth 1 lst2)) (not (eql (nth 2 lst1) (nth 2 lst2)))) T) ;dia dif, hora dif, id dif
		  ((and (= (nth 0 lst1) (nth 0 lst2)) (= (nth 1 lst1) (nth 1 lst2)) (eql (nth 2 lst1) (nth 2 lst2))) T) 		;mm dia, mm hora, mm id
		  ((and (/= (nth 0 lst1) (nth 0 lst2)) (= (nth 1 lst1) (nth 1 lst2)) (not (eql (nth 2 lst1) (nth 2 lst2)))) T) 	;dia dif, mm hora, id dif
		  (t NIL))
)


;MY-REMOVE
;recebe um elemento e uma lista
;remove esse elemento da lista
(defun my-remove (x l)
  (cond ((null l) nil) 
        (t (if (equal x (car l))  
               (my-remove x (cdr l))  
               (cons (car l) (my-remove x (cdr l)))))))

;POE-POR-ORDEM
;funcao que recebe uma subtarefa e um alista e coloca a subtarefa por ordem crescente de dia e hora
(defun poe-por-ordem (subt lista)
	(let ((n 1))
		(dolist (subt2 lista)
			(if (or (and (< (nth 0 subt) (nth 0 subt2)) (< (nth 1 subt) (nth 1 subt2))) (< (nth 0 subt) (nth 0 subt2)))
					
				(progn 
					(setf lista (insere-nth-lista subt lista n))
					(return-from poe-por-ordem lista)
				)
				(setf n (+ 1 n)))
		)
		(setf lista (insere-nth-lista subt lista n))
		(return-from poe-por-ordem lista)
	)
)
			   
;COLOCA ELEMENTO POR ORDEM
;Coloca um elemento na lista por ordem
(defun coloca-el-por-ordem (el lista)
	(let ((n 1))
		(dolist (e lista)
			(if (< (no-g el) (no-g e))
				(progn 
					(setf lista (insere-nth-lista el lista n))
					(return-from coloca-el-por-ordem lista)
				)
				(setf n (+ 1 n)))
		)
		(setf lista (insere-nth-lista el lista n))
		(return-from coloca-el-por-ordem lista)
	)
)		

;COLOCA LISTA POR ORDEM
;Coloca uma lista na outra, com os elementos ordenados
(defun coloca-por-ordem (sucessores sucessores_gerados)
	(dolist (ger sucessores_gerados)
		(setf sucessores(coloca-el-por-ordem ger sucessores))
	)
	(return-from coloca-por-ordem sucessores)
)

;COLOCA ELEMENTO POR ORDEM - GANANCIOSA
;Coloca um elemento na lista por ordem
(defun coloca-el-por-ordem-gananciosa (el lista)
	(let ((n 1))
		(dolist (e lista)
			(if (< (no-h el) (no-h e))
				(progn 
					(setf lista (insere-nth-lista el lista n))
					(return-from coloca-el-por-ordem-gananciosa lista)
				)
				(setf n (+ 1 n)))
		)
		(setf lista (insere-nth-lista el lista n))
		(return-from coloca-el-por-ordem-gananciosa lista)
	)
)		

;COLOCA LISTA POR ORDEM - GANANCIOSA
;Coloca uma lista na outra, com os elementos ordenados
(defun coloca-por-ordem-gananciosa (sucessores sucessores_gerados)
	(dolist (ger sucessores_gerados)
		(setf sucessores(coloca-el-por-ordem-gananciosa ger sucessores))
	)
	(return-from coloca-por-ordem-gananciosa sucessores)
)
 
 
;COLOCA ELEMENTO POR ORDEM - A*
;Coloca um elemento na lista por ordem
(defun coloca-el-por-ordem-A (el lista)
	(let ((n 1))
		(dolist (e lista)
			(if (< (+ (no-g el) (no-h el)) 
					(+ (no-g e) (no-h e)))
				(progn 
					(setf lista (insere-nth-lista el lista n))
					(return-from coloca-el-por-ordem-A lista)
				)
				(setf n (+ 1 n)))
		)
		(setf lista (insere-nth-lista el lista n))
		(return-from coloca-el-por-ordem-A lista)
	)
)		

;COLOCA LISTA POR ORDEM -A*
;Coloca uma lista na outra, com os elementos ordenados
(defun coloca-por-ordem-A (sucessores sucessores_gerados)
	(dolist (ger sucessores_gerados)
		(setf sucessores(coloca-el-por-ordem-A ger sucessores))
	)
	(return-from coloca-por-ordem-A sucessores)
)

;VERIFICA-SOLUCAO
;verifica se o estado recebido e solucao final
(defun verifica-solucao1 (estado)
	(let ((x (no-tarefas estado)) (estado-actual (no-estado estado)) (ntarefa 0))
		(dolist (alt estado-actual)
			(dolist (tar x)
				(if (pertence-a-lista alt tar)
					(setq x (elimina-nth-lista (+ 1 ntarefa) x)) 
					(setq ntarefa (+ 1 ntarefa))
				)
			)
			(setq ntarefa 0)
		)
		(setf (no-tarefas estado) x)
		(if (not x) (return-from verifica-solucao1 T) (return-from verifica-solucao1 NIL))
	)
) 

;VERIFICA SE E SOLUCAO - CPS
;verifica se uma lista é solucao para o problema de satisfaçao de restriçoes
(defun verifica-se-e-solucao (lista)
	(dolist (alt lista)
		(if (equal 'T alt) (return-from verifica-se-e-solucao NIL))
	)
	(return-from verifica-se-e-solucao T)
)
