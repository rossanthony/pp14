#lang racket
; • Repeatedly do the following:
;   ◦ Find a location containing a singleton set (a set containing just one number).
;     ▪ For every other set in the same row, the same column, or the same 3x3 box, remove that number (if present).
;   ◦ Find a number in a set that does not occur in any other set in the same row (or column, or box).
;     ▪ Reduce that set to a singleton containing that one number.
; • Quit when every set is a singleton, or when no more numbers can be removed from any set.

(define puzzle '((0 2 5 0 0 1 0 0 0)
                 (1 0 4 2 5 0 0 0 0)
                 (0 0 6 0 0 4 2 1 0)
                 (0 5 0 0 0 0 3 2 0)
                 (6 0 0 0 2 0 0 0 9)
                 (0 8 7 0 0 0 0 6 0)
                 (0 9 1 5 0 0 6 0 0)
                 (0 0 0 0 7 8 1 0 3)
                 (0 0 0 6 0 0 5 9 0)
                ))

(define possible-values (set 1 2 3 4 5 6 7 8 9))

(define test-row (list
  (set 9)
  (set 2)
  (set 5)
  (set 1 2 3 4 5 6 7 8 9)
  (set 1 2 3 4 5 6 7 8 9)
  (set 1)
  (set 1 2 3 4 5 6 7 8 9)
  (set 1 2 3 4 5 6 7 8 9)
  (set 1 2 3 4 5 6 7 8 9)
  ))


; (transform matrix)
; takes a Sudoku puzzle in the above list of lists format, and replaces each integer with a set of integer(s), 
; thus returning a list of lists containing sets of integers. Zeros in the matrix are replaced by the set
; Each zero cell in the grid needs to be assigned with a set of possible values 1-9, which can then be 
; whittled down through deduction

(define (transform matrix)
  (define (transform-helper matrix new-matrix)
    (cond
      [(empty? matrix) new-matrix]
      [(list? (car matrix)) 
       (cons (transform-helper (car matrix) new-matrix) (transform-helper (cdr matrix) new-matrix))]
      [(= 0 (car matrix)) 
       (cons possible-values (transform-helper (cdr matrix) new-matrix))]
      [else (cons (set (car matrix)) (transform-helper (cdr matrix) new-matrix))]))
  (transform-helper matrix '()))


; (solve matrix)
; accepts a Sudoku puzzle given as a list of lists, with each sub-list representing one row of the puzzle. 
; The elements of the sub-list are the integers shown in the puzzle, but with 0 representing a blank space. 
; Thus, the above puzzle would be represented as  
;   [[0 2 5 0 0 1 0 0 0] ... [0 0 0 6 0 0 5 9 0]]
; The value returned by solve will be a similar list of lists, but with all the zeros replaced by the 
; appropriate numbers 1..9. (Any unsolved locations-- there shouldn't be any — will be a set of the digits 
; that might be there.)

(define (solve matrix)
  ; (set-first (car (car (reverse (search-row-sets (transform matrix))))))
  ; (car (reverse (search-row-sets (transform matrix))))
   (reverse (search-row-sets (transform matrix)))
  )


(define (search-row-sets matrix)
    (define (row-sets-helper matrix new-matrix)
    (cond
      [(empty? matrix) new-matrix]
      [(= 1 (set-count (car (car matrix))))
          ; pass car of matrix (current row) into a function which searches and removes the known number from the sets of possible numbers in this row
        (row-sets-helper (cdr matrix) (cons (eliminate-row-sets (car matrix) (set-first (car (car matrix)))) new-matrix))
        ]
      [else (row-sets-helper (cdr matrix) (cons (car matrix) new-matrix))]
      ))
  (row-sets-helper matrix '()))


; Takes a set and filters out a given known number
(define (eliminate-row-sets row known-number)
  (define (eliminate-row-sets-helper row known-number new-row)
    (cond
      [(empty? row) new-row]
      [(and (> (set-count (car row)) 1) (set-member? (car row) known-number))
       ; should only enter here if its not already a single set and a known number is found in a set of possible numbers
       (eliminate-row-sets-helper (cdr row) known-number (cons (set-remove (car row) known-number) new-row))]
       
      [else (eliminate-row-sets-helper (cdr row) known-number (cons (car row) new-row))]
     ))
  (reverse (eliminate-row-sets-helper row known-number '())))

;(eliminate-row-sets 1 test-row)


; Takes a row as a list of sets
;(define (analyse-row row)
;  (define (analyse-row-helper row new-row)
;    (cond
;      [(empty? row) new-row]
;      [else (analyse-row-helper (cons (cdr row) '()) (eliminate-row-sets row (set-first (car row))))]
;      ))
;  (reverse (analyse-row-helper row '())))

;(analyse-row test-row)


; transform is now working as expected...
(transform puzzle)

;(solve puzzle)