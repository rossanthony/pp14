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
       (cons (set 1 2 3 4 5 6 7 8 9) (transform-helper (cdr matrix) new-matrix))]
      [else (cons (set (car matrix)) (transform-helper (cdr matrix) new-matrix))]))
  (transform-helper matrix '()))


