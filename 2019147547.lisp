(defun printBoard (sudokuBoard) ;;Print Board
   (format nil "~%~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%~a~%"
        " ----------------------- "
        "|" (elt (elt sudokuBoard 0) 0) (elt (elt sudokuBoard 0) 1) (elt (elt sudokuBoard 0) 2) "|" (elt (elt sudokuBoard 0) 3) (elt (elt sudokuBoard 0) 4) (elt (elt sudokuBoard 0) 5) "|" 
         (elt (elt sudokuBoard 0) 6) (elt (elt sudokuBoard 0) 7) (elt (elt sudokuBoard 0) 8) "|"
        "|" (elt (elt sudokuBoard 1) 0) (elt (elt sudokuBoard 1) 1) (elt (elt sudokuBoard 1) 2) "|" (elt (elt sudokuBoard 1) 3) (elt (elt sudokuBoard 1) 4) (elt (elt sudokuBoard 1) 5) "|" 
         (elt (elt sudokuBoard 1) 6) (elt (elt sudokuBoard 1) 7) (elt (elt sudokuBoard 1) 8) "|"
        "|" (elt (elt sudokuBoard 2) 0) (elt (elt sudokuBoard 2) 1) (elt (elt sudokuBoard 2) 2) "|" (elt (elt sudokuBoard 2) 3) (elt (elt sudokuBoard 2) 4) (elt (elt sudokuBoard 2) 5) "|" 
         (elt (elt sudokuBoard 2) 6) (elt (elt sudokuBoard 2) 7) (elt (elt sudokuBoard 2) 8) "|"
        "|-------+-------+-------|"
        "|" (elt (elt sudokuBoard 3) 0) (elt (elt sudokuBoard 3) 1) (elt (elt sudokuBoard 3) 2) "|" (elt (elt sudokuBoard 3) 3) (elt (elt sudokuBoard 3) 4) (elt (elt sudokuBoard 3) 5) "|" 
         (elt (elt sudokuBoard 3) 6) (elt (elt sudokuBoard 3) 7) (elt (elt sudokuBoard 3) 8) "|"
        "|" (elt (elt sudokuBoard 4) 0) (elt (elt sudokuBoard 4) 1) (elt (elt sudokuBoard 4) 2) "|" (elt (elt sudokuBoard 4) 3) (elt (elt sudokuBoard 4) 4) (elt (elt sudokuBoard 4) 5) "|" 
         (elt (elt sudokuBoard 4) 6) (elt (elt sudokuBoard 4) 7) (elt (elt sudokuBoard 4) 8) "|"
        "|" (elt (elt sudokuBoard 5) 0) (elt (elt sudokuBoard 5) 1) (elt (elt sudokuBoard 5) 2) "|" (elt (elt sudokuBoard 5) 3) (elt (elt sudokuBoard 5) 4) (elt (elt sudokuBoard 5) 5) "|" 
         (elt (elt sudokuBoard 5) 6) (elt (elt sudokuBoard 5) 7) (elt (elt sudokuBoard 5) 8) "|"
        "|-------+-------+-------|"
        "|" (elt (elt sudokuBoard 6) 0) (elt (elt sudokuBoard 6) 1) (elt (elt sudokuBoard 6) 2) "|" (elt (elt sudokuBoard 6) 3) (elt (elt sudokuBoard 6) 4) (elt (elt sudokuBoard 6) 5) "|" 
         (elt (elt sudokuBoard 6) 6) (elt (elt sudokuBoard 6) 7) (elt (elt sudokuBoard 6) 8) "|"
        "|" (elt (elt sudokuBoard 7) 0) (elt (elt sudokuBoard 7) 1) (elt (elt sudokuBoard 7) 2) "|" (elt (elt sudokuBoard 7) 3) (elt (elt sudokuBoard 7) 4) (elt (elt sudokuBoard 7) 5) "|" 
         (elt (elt sudokuBoard 6) 7) (elt (elt sudokuBoard 7) 7) (elt (elt sudokuBoard 7) 8) "|"
        "|" (elt (elt sudokuBoard 8) 0) (elt (elt sudokuBoard 8) 1) (elt (elt sudokuBoard 8) 2) "|" (elt (elt sudokuBoard 8) 3) (elt (elt sudokuBoard 8) 4) (elt (elt sudokuBoard 8) 5) "|" 
         (elt (elt sudokuBoard 8) 6) (elt (elt sudokuBoard 8) 7) (elt (elt sudokuBoard 8) 8) "|"
        " ----------------------- "
   )
)

(defun test()
    
(calculate '((0 3 5 4 6 9 2 7 8) (7 8 2 1 0 5 6 0 9) (0 6 0 2 7 8 1 3 5) (3 2 1 0 4 6 8 9 7) (8 0 4 9 1 3 5 0 6) (5 9 6 8 2 0 4 1 3) (9 1 7 6 5 2 0 8 0) (6 0 3 7 0 1 9 5 2) (2 5 8 3 9 4 7 6 0))
 '((0 0) (1 4) (1 7) (2 0) (2 2) (3 3) (4 1) (4 7) (5 5) (6 6) (6 8) (7 1) (7 4) (8 8)))  

)

(defun checkBoard(sudokuBoard xIndex yIndex) ;;Check Board
    (and
        (not (checkDuplicate (xList sudokuBoard xIndex yIndex) (coordinate sudokuBoard xIndex yIndex)))
        (not (checkDuplicate (yList sudokuBoard xIndex yIndex) (coordinate sudokuBoard xIndex yIndex)))
        (not (checkDuplicate (boxList sudokuBoard xIndex yIndex) (coordinate sudokuBoard xIndex yIndex)))
    )
)

(defun solveBoard(sudokuBoard) ;;Solve Board
    (printBoard (calculate sudokuBoard (modify2D (emptyCheck (twoDtoOneD sudokuBoard) '() 0) '())))
)

(defun calculate(listval index) ;;Find out what value fits in the empty place (0)
    (cond
        ((null index)
            listval
        )
        (t  
            (cond
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 1) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 1)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 2) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 2)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 3) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 3)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 4) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 4)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 5) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 5)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 6) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 6)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 7) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 7)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 8) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 8)
                )
                ((checkBoard (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 9) (elt (car index) 0) (elt (car index) 1))
                    (replaceAtom listval (elt (car index) 1) (elt (car index) 0) 9)
                )            
            )
            (calculate listval (cdr index))
        )
    )
)

(defun replaceAtom(listval x y n) ;;Replace an Atom in a Specific List
    (setf (elt (elt listval y) x) n)
    listval
)

(defun modify2D(listval l);;Change 1D Indices to 2D Indices
    (cond
        ((null listval)
            l
        )
        (t
            (setq l (cons (index (car listval)) l))
            (modify2D (cdr listval) l)
        )
    )
)

(defun emptyCheck(listval l c) ;;Returns List of Indices of Zero
    (cond
        ((eq c (length listval))  
            l
        )
        (t
            (if (eq (elt listval c) 0)
                (setq l (cons c l))
            )
            (emptyCheck listval l (1+ c))
        )
    )
)

(defun twoDtoOneD(listval) ;;Modify 2D list to 1D list
    listval (concatenate 'list (elt listval 0) (elt listval 1) (elt listval 2) (elt listval 3) (elt listval 4) (elt listval 5) (elt listval 6) (elt listval 7) (elt listval 8))
)

(defun index(n) ;;Returns index of 2D list
    (list (floor(/ n 9)) (mod n 9))
)

(defun coordinate(listval x y) ;;Return a Value at a Certain Coordinate
    (elt (elt listval x) y)
)

(defun checkList(listval a) ;;Check if the Value is Being Repeated
    (cond 
        ((null listval)  ;;Base Case
            0
        ) 
        ((eq (car listval) a) 
            (1+ (checkList (cdr listval) a))
        )
        (t 
            (checkList(cdr listval) a)
        )
    )
)

(defun checkDuplicate(listval a) ;;Returns T if a list contains duplicates
    (cond 
        ((> (checkList listval a) 1)
            t
        )
        ((= (checkList listval a) 1)
            nil
        )
    )
)

(defun yList(listval x y) ;;Create Y-List
    (list (elt (elt listval 0) y) (elt (elt listval 1) y) (elt (elt listval 2) y) (elt (elt listval 3) y) (elt (elt listval 4) y) (elt (elt listval 5) y) 
        (elt (elt listval 6) y) (elt (elt listval 7) y) (elt (elt listval 8) y))
)

(defun xList(listval x y);;Create X-List
    (elt listval x)
)

(defun boxList(listval x y) ;;Create Box List
    (case (quadrantcheck x y)
        (1 (list (elt (elt listval 0) 0) (elt (elt listval 1) 0) (elt (elt listval 2) 0) (elt (elt listval 0) 1) (elt (elt listval 1) 1) (elt (elt listval 2) 1) 
        (elt (elt listval 0) 2) (elt (elt listval 1) 2) (elt (elt listval 2) 2)))
        (2 (list (elt (elt listval 0) 3) (elt (elt listval 1) 3) (elt (elt listval 2) 3) (elt (elt listval 0) 4) (elt (elt listval 1) 4) (elt (elt listval 2) 4) 
        (elt (elt listval 0) 5) (elt (elt listval 1) 5) (elt (elt listval 2) 5)))
        (3 (list (elt (elt listval 0) 6) (elt (elt listval 1) 6) (elt (elt listval 2) 6) (elt (elt listval 0) 7) (elt (elt listval 1) 7) (elt (elt listval 2) 7) 
        (elt (elt listval 0) 8) (elt (elt listval 1) 8) (elt (elt listval 2) 8)))
        (4 (list (elt (elt listval 3) 0) (elt (elt listval 4) 0) (elt (elt listval 5) 0) (elt (elt listval 3) 1) (elt (elt listval 4) 1) (elt (elt listval 5) 1) 
        (elt (elt listval 3) 2) (elt (elt listval 4) 2) (elt (elt listval 5) 2)))
        (5 (list (elt (elt listval 3) 3) (elt (elt listval 4) 3) (elt (elt listval 5) 3) (elt (elt listval 3) 4) (elt (elt listval 4) 4) (elt (elt listval 5) 4) 
        (elt (elt listval 3) 5) (elt (elt listval 4) 5) (elt (elt listval 5) 5)))
        (6 (list (elt (elt listval 3) 6) (elt (elt listval 4) 6) (elt (elt listval 5) 6) (elt (elt listval 3) 7) (elt (elt listval 4) 7) (elt (elt listval 5) 7) 
        (elt (elt listval 3) 8) (elt (elt listval 4) 8) (elt (elt listval 5) 8)))
        (7 (list (elt (elt listval 6) 0) (elt (elt listval 7) 0) (elt (elt listval 8) 0) (elt (elt listval 6) 1) (elt (elt listval 7) 1) (elt (elt listval 8) 1) 
        (elt (elt listval 6) 2) (elt (elt listval 7) 2) (elt (elt listval 8) 2)))
        (8 (list (elt (elt listval 6) 3) (elt (elt listval 7) 3) (elt (elt listval 8) 3) (elt (elt listval 6) 4) (elt (elt listval 7) 4) (elt (elt listval 8) 4) 
        (elt (elt listval 6) 5) (elt (elt listval 7) 5) (elt (elt listval 8) 5)))
        (9 (list (elt (elt listval 6) 6) (elt (elt listval 7) 6) (elt (elt listval 8) 6) (elt (elt listval 6) 7) (elt (elt listval 7) 7) (elt (elt listval 8) 7) 
        (elt (elt listval 6) 8) (elt (elt listval 7) 8) (elt (elt listval 8) 8)))
    )
)

(defun quadrantCheck(x y) ;;Find Out What Quadrant the Variable is Located
    (cond
        ((and(> y 5) (> x 5)) 9)
        ((and(> y 2) (> x 5)) 8)
        ((> x 5) 7)
        ((and(> y 5) (> x 2)) 6)
        ((and(> y 2) (> x 2)) 5)
        ((> x 2) 4)
        ((> y 5) 3)
        ((> y 2) 2)
        (t 1)
    )
)