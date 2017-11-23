; Aufgabe 1
;----------

; (a) Elemente tauschen: Schreiben Sie eine Funktion rotiere, die eine Liste 
; als Argument erhäalt und eine neue Liste zuruckliefert, in der das 
; vormals erste Element nun das letzte ist.
(defun rotiere (arg1)
    (append(rest arg1) (list (first arg1)))
)

; (b) Element einfugen : Schreiben Sie eine Funktion neues-vorletztes , 
; die eine Liste als Argument erhält und eine neue Liste zuruckliefert, 
; in der das vormals erste Element nun das letzte ist.
(defun abl (list)
    (loop for l on list
        while (rest l)
            collect (first l))
)

(defun nve(e l) 
    (append (append (abl l) (list e) (last l)))
)

; (c) Länge einer Liste berechnen: Schreiben Sie eine Funktion my-length
; zur Berechnung der Länge einer Liste.
(defun my-length (list)
    (if list
        (1+ (my-length (cdr list)))
    0)
)

; (d) Länge einer geschachtelten Liste berechnen: Schreiben Sie eine Funktion 
; my-lengthR zur Berechnung der Länge einer Liste und aller eingeschachtelten
; Listen.
(defun my-lengthR (list)
    (if list
        (if (listp list)
            (1+ (my-lengthR (cdr list)))
        0)
        (1+ (my-lengthR (cdr list)))
    0)
) ; TODO Listen mit mehreren Ebenen

; (e) Listen umkehren: Schreiben eine Funktion my-reverse zum Umkehren 
; einer Liste.
(defun my-reverse (list)
    (cond
        ((null list) '())
        (T 
            (append (my-reverse (cdr list))
            (list (car list)))
        )
	)
)

; (f) Geschachtelte Listen umkehren: Schreiben eine Funktion my-reverseR 
; zum Umkehren einer Liste.

; Aufgabe 2
;----------

; (a) Darstellung eines Binärbaums: Überlegen Sie, wie Sie mittels einer Liste 
; einen Binärbaum darstellen können.


; (b) Baumtraversierung: Schreiben Sie 3 Funktionen zum Traversieren eines 
; Binärbaums, bei der auch die Knoteninhalte ausgegeben werden.

;inorder

;postorder

;preorder