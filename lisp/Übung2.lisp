;Dimitry Nagorny, 1410351, repo-50
;Philipp Minges, 1426312, repo-52

; Baumstruktur = Liste aus drei Elementen
(defun create-tree-structure (root left right)
    (list root left right)
)

; Wurzel
(defun root (tree)
    (car tree)
)

; linker Nachfolger
(defun left-follower (tree)
    (cadr tree)
)

; rechter Nachfolger
(defun right-follower (tree)
    (caddr tree)
)

; Operationen
; -----------

; erstelle Baum aus Liste
(defun create-tree (list)
    (if (not (boundp 'tree))
        (setf tree nil)
    )
    (dolist (node list)
        (setf tree (tree-insertNode node tree)) ; füge aktuelles Element in Baum ein
    )
)

; füge Knoten in Baum ein
(defun tree-insertNode (node tree)
    (cond 
        ((null tree) (create-tree-structure node nil nil))
        ((= node (root tree)) tree)
        ((< node (root tree)) (create-tree-structure (root tree) (tree-insertNode node (left-follower tree)) (right-follower tree)))
        ((> node (root tree)) (create-tree-structure (root tree) (left-follower tree) (tree-insertNode node (right-follower tree))))
    )
)

(create-tree '(3 5 8 4 2))
(print "Binärbaum (3 5 8 4 2):")
(print tree)

(print "Insert 1 in Binärbaum (3 5 8 4 2):")
(setf tree (tree-insertNode 1 tree))
(print tree)

; füge Werte aus Liste in Baum ein
(defun tree-insertListValues (li tri)
	(if li
		(let ((newTree (tree-insertNode (first li) tri)))
			(tree-insertListValues (rest li) newTree)
		)
		tri
	)
)

; Werte aus Datei in Liste umwandeln
(defun getListFromFile (filePath)
	(with-open-file (in filePath)
		(loop for val = (read in nil)
			while val
				collect val
		)
	)
)

; füge Werte aus Datei in Baum ein
(defun tree-insertFileValues (filePath tri)
    (let ((listValues (getListFromFile filePath)))
		(tree-insertListValues listValues tri)
	)
)

(setf tree (tree-insertFileValues "./nodes.txt" tree))
(print "Insert von 10 20 30 aus der Datei nodes.txt zum Tree:")
(print tree)

; Contains
(defun tree-containsValue (node tree)
	(if tree
		(cond 
			((= node (root tree)) T)
			((< node (root tree)) (tree-containsValue node (left-follower tree)))
			((> node (root tree)) (tree-containsValue node (right-follower tree)))
		)
		nil
	)
)
(print "Ist 7 im Baum?")
(print (if (tree-containsValue 7 tree) "Ja." "Nein."))
(print "Ist 2 im Baum?")
(print (if (tree-containsValue 2 tree) "Ja." "Nein."))

; Size
(defun tree-size (tree)
    (cond ((null tree) 0)
        ((listp (root tree)) 
            (+ (tree-size (root tree)) (tree-size (left-follower tree)) (tree-size (right-follower tree))) 
        ) 
        (T
            (+ 1 (tree-size (left-follower tree)) (tree-size (right-follower tree)))
        )
    )
)
(print "Anzahl aller Knoten im Baum:")
(print (tree-size tree))

; getMax
(defun tree-maxValue (tree)
    (if (right-follower tree)
		(tree-maxValue (right-follower tree))
		(root tree)
	)
)

(print "Biggest Wert im Baum:")
(print (tree-maxValue tree))

; getMin
(defun tree-minValue (tree)
    (if (left-follower tree)
		(tree-minValue (left-follower tree))
		(root tree)
	)
)
(print "Kleinster Wert im Baum:")
(print (tree-minValue tree))

; isEmpty
(defun tree-isEmpty (tree)
    (if (root tree)
		nil
		T
	)
)

(print "Ist aktueller Baum leer?")
(print (if (tree-isEmpty tree) "Ja." "Nein."))
(print "Ist Baum (NIL NIL NIL) leer?")
(print (if (tree-isEmpty '(NIL NIL NIL)) "Ja." "Nein."))

; height
(defun tree-height (tree)
	(if (tree-isEmpty tree)
		0
		(let ((leftHeight (+ 1 (tree-height (left-follower tree)))))
			(let ((rightHeight (+ 1 (tree-height (right-follower tree)))))
				(if (> leftHeight rightHeight)
					leftHeight
					rightHeight
				)
			)
		)
	)
)
(print "Height des aktuellen Baumes:")
(print (tree-height tree))
(print "Height des Baumes (4 (3 NIL NIL) (5 NIL NIL)):")
(print (tree-height '(4 (3 NIL NIL) (5 NIL NIL))))
(print "Height des Baumes (4 (3 (2 NIL NIL) NIL) (5 NIL NIL)):")
(print (tree-height '(4 (3 (2 NIL NIL) NIL) (5 NIL NIL))))

; remove
(defun tree-remove (val tree)
	(cond
		((null (root tree)) nil)
    	((and (= val (root tree)) (= (tree-height tree) 1)) nil)
    	((and (= val (root tree)) (null (right-follower tree)))
        	(create-tree-structure (tree-maxValue (left-follower tree)) (remove (left-follower tree) (tree-maxValue (left-follower tree))) nil)
    	)
    	(
      		(= val (root tree))
        	(create-tree-structure (tree-minValue (right-follower tree)) (left-follower tree) (tree-remove (tree-minValue (right-follower tree)) (right-follower tree)))
    	)
    	(
      	(< val (root tree))
        	(create-tree-structure (root tree) (tree-remove val (left-follower tree)) (right-follower tree))
    	)
    	(
      	(> val (root tree))
        	(create-tree-structure (root tree) (left-follower tree) (tree-remove val (right-follower tree)))
    	)
	)
)

(print "Entferne 7 aus dem Baum:")
(print (tree-remove 7 tree))
(print "Entferne 2 aus dem Baum:")
(print (tree-remove 2 tree))

; addAll
(defun tree-addAll (otherTree tri)
	(if (not (tree-isEmpty otherTree))
		(let ((newTree (tree-insertNode (root otherTree) tri)))
			(let ((newTree2 (tree-addAll (left-follower otherTree) newTree)))
				(tree-addAll (right-follower otherTree) newTree2)
			)
		)
		tri
	)
)
(print "Adde dem aktuellem Baum den Baum (3 (0 NIL NIL) (11 NIL NIL)) hinzu:")
(setf tree (tree-addAll '(3 (0 NIL NIL) (11 NIL NIL)) tree))
(print tree)

; levelorder
; Helper for levelorder
(defun curLevel(cur tree)
    (if (endp tree) nil
        (if (= 1 cur) 
            (list (car tree))
            (append 
                (curLevel (- cur 1)(cadr tree)) 
                (curLevel (- cur 1)(caddr tree))
            )
        )
    )
)

; print levelorder - iterativ :(
(defun levelorder (tree)
    (setq res nil)
    (loop for i from 1 to (tree-height tree)
        do (setq res (append res (curLevel i tree))
        )
  )
  res
)

(print "Levelorder vom aktuellen Baum:")
(levelorder tree)