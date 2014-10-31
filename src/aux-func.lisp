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
;                                      FUNCOES AUXILIARES                                  ;
; ======================================================================================== ;

; Funcoes de auxilio a implementacao do jogo Fill-a-Pix.
		

; ELIMINA O NTH DA LISTA

; Elimina item da posição nth da lst.
; O primeiro elemento da lst é 1.

(defun list-nth-delete(n list)
  (append (subseq list 0 (1- n)) (nthcdr n list)))


; INSERE O NTH DA LISTA

; Função que recebe um elemento, uma lst e uma posição 
; e retorna uma nova lst que contém o elemento na      
; desejada.
; O primeiro elemento da lst é 1.

(defun list-nth-insert(elem lst pos)
    (if (or (eql pos 1) (eql lst nil))
            (cons elem lst)
            (cons (car lst) (list-nth-insert elem (cdr lst) (- pos 1)))))


; ELIMINA ULTIMO

; Retorna a mesma lst, mas sem o ultimo elemento.

(defun list-last-delete(lst)
  (cond ((null lst) nil)
        ((null (cdr lst)) nil) 
        (t (cons (car lst) (list-last-delete (cdr lst)))))) 


; ELEMENTO EM LISTA

; Retorna true se x belongsr a lst l.

(defun list-elem-in(elem lst)
  (cond ((null lst) NIL)
        (t (or (equal el (car lst)) (belongs-a-lst el (cdr lst)))))

)


; LISTAS IGUAIS

; Recebe duas lsts (subtarefas) e verifica se sao iguais.

(defun list-equal (lst1 lst2)
	(let ((answer T))
		(if (equal (list-length lst1) (list-length lst2))
			(if (null lst1) (setf answer NIL) (if (null lst2) (setf answer NIL)
				(cond ((and (= (nth 0 lst1) (nth 0 lst2))
				      (= (nth 1 lst1) (nth 1 lst2)) (eql (nth 2 lst1) (nth 2 lst2)))
				        (setf answer T)) 
					   (t (setf answer NIL)))
				))
			(setf answer NIL)
		)
		(return-from list-equal answer)
	)
)


; ALTERNATIVAS IGUAIS

; Retorna true se alt1 e alt2 forem iguais.

(defun equal-alt (alt1 alt2)
	(let ((list-equal T) (i 0))
		(loop	
			(if (equal (list-length alt1) (list-length alt2))
				(progn 						
					(if (list-equal (nth i alt1) (nth i alt2))
						(progn 
							(setf list-equal T) 
							(setf i (+ 1 i)) 
							(if (>= i (- (list-length alt1) 1)) 
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


; ESTADOS IGUAIS

; Retorna true se os dois states recebidos como argumentos sao iguais, false caso contrario.

(defun stats-equal (sta1 sta2)
	(let ((lst-aux ()) (i 0))
		(if (equal (list-length sta1) (list-length sta2))
			(progn
				(setf lst-aux (make-list (list-length sta1) :initial-element 'NIL))
				(dolist (alt1 sta1)
					(dolist (alt2 sta2)
						(if (equal-alt alt1 alt2)
							(progn
								(setf lst-aux (list-nth-delete (+ 1 i) lst-aux))
								(setf lst-aux (list-nth-insert 'T lst-aux (+ 1 i)))
							)
						)
					)
					(setf i (+ 1 i))
				)
			)
			(return-from stats-equal NIL)
		)
		(dolist (aux lst-aux)
			(if (equal aux 'NIL)
				(return-from stats-equal NIL)
			)
		)
		(return-from stats-equal T)
	)
)


; PROCURA NA LISTA DE ESTADOS

; Recebe um state e retorna true se o state estiver na lst-states.

(defun list-is-in (state lst-states)
	(let ((belongs T) (state-is (in-state state)))
			(dolist (in-lst lst-states)
				(if (and state-is in-lst)
					(progn
						(if (stats-equal state-is (in-state in-lst))
							(progn (setf belongs T) (return-from list-is-in belongs))
							(setf belongs NIL)) 
					)
				)
			)
		(return-from list-is-in belongs)
	)
)


; TAREFA VALIDA

; Recebe duas subtarefas e checks se sao validas juntas.

(defun checks (lst1 lst2)
	(cond ((and (= (nth 0 lst1) (nth 0 lst2)) (/= (nth 1 lst1) (nth 1 lst2)) (not (eql (nth 2 lst1) (nth 2 lst2)))) T)
		  ((and (/= (nth 0 lst1) (nth 0 lst2)) (/= (nth 1 lst1) (nth 1 lst2)) (not (eql (nth 2 lst1) (nth 2 lst2)))) T)
		  ((and (= (nth 0 lst1) (nth 0 lst2)) (= (nth 1 lst1) (nth 1 lst2)) (eql (nth 2 lst1) (nth 2 lst2))) T)
		  ((and (/= (nth 0 lst1) (nth 0 lst2)) (= (nth 1 lst1) (nth 1 lst2)) (not (eql (nth 2 lst1) (nth 2 lst2)))) T)
		  (t NIL))
)


; REMOVE ELEMENTO DA LISTA

; Recebe um elemento e uma lst.
; Remove esse elemento da lst.

(defun my-remove (x l)
  (cond ((null l) nil) 
        (t (if (equal x (car l))  
               (my-remove x (cdr l))  
               (cons (car l) (my-remove x (cdr l)))))))
               

; ORDENA LISTA

; Funcao que recebe uma subtarefa e um alst e coloca a subtarefa por ordem crescente de dia e hora.

(defun list-sort (subt lst)
	(let ((n 1))
		(dolist (subt2 lst)
			(if (or (and (< (nth 0 subt) (nth 0 subt2))
			             (< (nth 1 subt) (nth 1 subt2)))
              (< (nth 0 subt) (nth 0 subt2)))
					
				(progn 
					(setf lst (list-nth-insert subt lst n))
					(return-from list-sorte lst)
				)
				(setf n (+ 1 n)))
		)
		(setf lst (list-nth-insert subt lst n))
		(return-from list-sorte lst)
	)
)


; COLOCA ELEMENTO NA LISTA POR ORDEM

; Coloca um elemento na lst por ordem.

(defun list-insert-elem (el lst)
	(let ((n 1))
		(dolist (e lst)
			(if (< (in-g el) (in-g e))
				(progn 
					(setf lst (list-nth-insert el lst n))
					(return-from list-insert-elem lst)
				)
				(setf n (+ 1 n)))
		)
		(setf lst (list-nth-insert el lst n))
		(return-from list-insert-elem lst)
	)
)
