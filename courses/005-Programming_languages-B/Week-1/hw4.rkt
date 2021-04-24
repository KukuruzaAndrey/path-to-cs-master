
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; 1. Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be numbers.
; Further assume stride is positive. sequence produces a list of numbers from low to high (including
; low and possibly high) separated by stride and in sorted order. Sample solution: 4 lines. Examples:
; (print (sequence 3 11 2)) => ’(3 5 7 9 11)
; (print (sequence 3 8 3))  => ’(3 6)
; (print (sequence 3 2 1))  => ’()
(define (sequence low high stride)
    (if (>= high low)
        (cons low (sequence (+ low stride) high stride))
        null))

; 2. Write a function string-append-map that takes a list of strings xs and a string suffix and returns a
; list of strings. Each element of the output should be the corresponding element of the input appended
; with suffix (with no extra space between the element and suffix). You must use Racket-library
; functions map and string-append. Sample solution: 2 lines.
; (print (string-append-map null "a")) => ’()
; (print (string-append-map (list "a" "b" "c") "a")) => ’("aa" "ba" "ca")
(define (string-append-map los suffix)
    (map (lambda (s) (string-append s suffix)) los))

; 3. Write a function list-nth-mod that takes a list xs and a number n. If the number is negative,
; terminate the computation with (error "list-nth-mod: negative number"). Else if the list is
; empty, terminate the computation with (error "list-nth-mod: empty list"). Else return the i-th
; element of the list where we count from zero and i is the remainder produced when dividing n by the
; list’s length. Library functions length, remainder, car, and list-tail are all useful – see the Racket
; documentation. Sample solution is 6 lines.
; (print (list-nth-mod (list 1 2 3) -1)) => (error "list-nth-mod: negative number")
; (print (list-nth-mod null 1)) => (error "list-nth-mod: empty list")
; (print (list-nth-mod (list 1 2 3) 0)) => 1
; (print (list-nth-mod (list 1 2 3) 2)) => 3
; (print (list-nth-mod (list 1 2 3) 5)) => 3
(define (list-nth-mod lst index)
    (cond [(< index 0) (error "list-nth-mod: negative number")]
          [(null? lst) (error "list-nth-mod: empty list")]
          [#t (car (list-tail lst (remainder index (length lst))))]))

; 4. Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding
; the first n values produced by s in order. Assume n is non-negative. Sample solution: 5 lines. Note:
; You can test your streams with this function instead of the graphics code.
; (print (stream-for-n-steps ones 0)) => ’()
; (print (stream-for-n-steps ones 4)) => ’(1 1 1 1)
; (print (stream-for-n-steps nats 4)) => ’(1 2 3 4)
(define (stream-for-n-steps stream number)
    (if (= number 0)
        null
        (letrec ([step (stream)]
                 [val (car step)]
                 [new-stream (cdr step)])
           (cons val (stream-for-n-steps new-stream (- number 1))))))

; 5. Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
; except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream
; is a thunk that when called produces a pair. Here the car of the pair will be a number and the cdr will
; be another stream.
; (print (stream-for-n-steps funny-number-stream 1)) => ’(1)
; (print (stream-for-n-steps funny-number-stream 11)) => ’(1 2 3 4 -5 6 7 8 9 -10 11)
(define funny-number-stream
    (letrec ([transform (lambda (x)
                            (if (= (remainder x 5) 0)
                            (- 0 x)
                            x))]
             [aux (lambda (x)
                    (cons (transform x) (lambda () (aux (+ x 1)))))])
        (lambda () (aux 1))))

; 6. Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg"
; and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
; when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
; and a thunk that when called... etc. Sample solution: 4 lines.
; (print (stream-for-n-steps dan-then-dog 1)) => ’("dan.jpg")
; (print (stream-for-n-steps dan-then-dog 4)) => ’("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg")
(define dan-then-dog
    (letrec ([dan (lambda () (cons "dan.jpg" dog))]
             [dog (lambda () (cons "dog.jpg" dan))])
        dan))

; 7. Write a function stream-add-zero that takes a stream s and returns another stream. If s would
; produce v for its i-th element, then (stream-add-zero s) would produce the pair (0 . v) for its
; i-th element. Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
; Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog)
; with place-repeatedly.
; (print (stream-for-n-steps (stream-add-zero ones) 3)) => ’((0 . 1) (0 . 1) (0 . 1))
; (print (stream-for-n-steps (stream-add-zero dan-then-dog) 3)) => ’((0 . "dan.jpg") (0 . "dog.jpg") (0 . "dan.jpg"))
(define (stream-add-zero s)
    (lambda ()
        (let ([step (s)])
            (cons (cons 0 (car step)) 
              (stream-add-zero (cdr step))))))

; 8. Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may or
; may not be the same length, but assume they are both non-empty. The elements produced by the
; stream are pairs where the first part is from xs and the second part is from ys. The stream cycles
; forever through the lists. For example, if xs is ’(1 2 3) and ys is ’("a" "b"), then the stream
; would produce, (1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"),
; (2 . "b"), etc.
; Sample solution is 6 lines and is more complicated than the previous stream problems. Hints: Use one
; of the functions you wrote earlier. Use a recursive helper function that takes a number n and calls
; itself with (+ n 1) inside a thunk.
; (print (stream-for-n-steps (cycle-lists (list 1) (list 1)) 3)) => ’((1 . 1) (1 . 1) (1 . 1))
; (print (stream-for-n-steps (cycle-lists (list 1) (list 1 2)) 4)) => ’((1 . 1) (1 . 2) (1 . 1) (1 . 2))
; (print (stream-for-n-steps (cycle-lists (list 1 2 3 ) (list "a" "b")) 8)) => 
; ’((1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"), (2 . "b"))
(define (cycle-lists xs ys)
    (letrec ([aux (lambda (index)
                     (let ([x (list-nth-mod xs index)]
                           [y (list-nth-mod ys index)])
                        (cons (cons x y)
                              (lambda () (aux (+ index 1))))))])
        (lambda () (aux 0))))

; 9. Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket’s
; assoc library function except (1) it processes a vector (Racket’s name for an array) instead of a list,
; (2) it allows vector elements not to be pairs in which case it skips them, and (3) it always takes exactly
; two arguments. Process the vector elements in order starting from 0. You must use library functions
; vector-length, vector-ref, and equal?. Return #f if no vector element is a pair with a car field
; equal to v, else return the first pair with an equal car field. Sample solution is 9 lines, using one local
; recursive helper function.
; (print (vector-assoc 5 (vector (cons 5 1)))) => (5 . 1)
; (print (vector-assoc 4 (vector (cons 5 1)))) => #f
; (print (vector-assoc 5 (vector (cons 4 2) 5 "a" (cons "a" 1) (cons 5 1)))) => (5 . 1)
(define (vector-assoc v vec)
    (letrec ([aux (lambda (index)
                    (if (< index (vector-length vec))
                        (let ([el (vector-ref vec index)])
                            (cond [(not (pair? el))    (aux (+ index 1))]
                                  [(equal? v (car el))                el]
                                  [#t                  (aux (+ index 1))]))
                        #f))])
        (aux 0)))

; 10. Write a function cached-assoc that takes a list xs and a number n and returns a function that takes
; one argument v and returns the same thing that (assoc v xs) would return. However, you should
; use an n-element cache of recent results to possibly make this function faster than just calling assoc
; (if xs is long and a few elements are returned often). The cache must be a Racket vector of length n
; that is created by the call to cached-assoc (use Racket library function vector or make-vector) and
; used-and-possibly-mutated each time the function returned by cached-assoc is called. Assume n is
; positive.
; The cache starts empty (all elements #f). When the function returned by cached-assoc is called, it
; first checks the cache for the answer. If it is not there, it uses assoc and xs to get the answer and if
; the result is not #f (i.e., xs has a pair that matches), it adds the pair to the cache before returning
; (using vector-set!). The cache slots are used in a round-robin fashion: the first time a pair is added
; to the cache it is put in position 0, the next pair is put in position 1, etc. up to position n − 1 and
; then back to position 0 (replacing the pair already there), then position 1, etc.
; (print ((cached-assoc (list (cons 5 1)) 1) 5)) => (5 . 1)!!!
; (print ((cached-assoc (list (cons 5 1)) 1) 4)) => #f
; (print ((cached-assoc (list (cons 4 2) (cons "a" 1) (cons 5 1)) 1) 5)) => (5 . 1)
; (print ((cached-assoc (list (cons 4 2) (cons "a" 1) (cons 5 1)) 30) 5)) => (5 . 1)
(define (cached-assoc xs n)
    (let ([memo (make-vector n #f)]
          [memo-index 0])
       (lambda (v)
           (let ([from-cache (vector-assoc v memo)])
              (if from-cache
                  from-cache
                  (let ([new-cached (assoc v xs)])
                     (begin (and new-cached 
                                 (vector-set! memo memo-index new-cached))
                                 (set! memo-index (if (= memo-index (- n 1)) 
                                                      0 
                                                      (+ 1 memo-index)))
                            new-cached)))))))

; 11. (Challenge Problem:) Define a macro that is used like (while-less e1 do e2) where e1 and e2
; are expressions and while-less and do are syntax (keywords). The macro should do the following:
; • It evaluates e1 exactly once.
; • It evaluates e2 at least once.
; • It keeps evaluating e2 until and only until the result is not a number less than the result of the
; evaluation of e1.
; • Assuming evaluation terminates, the result is #t.
; • Assume e1 and e2 produce numbers; your macro can do anything or fail mysteriously otherwise.
; Hint: Define and use a recursive thunk. Sample solution is 9 lines. Example:
; (define a 2)
; (while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
; (while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
; Evaluating the second line will print "x" 5 times and change a to be 7. So evaluating the third line
; will print "x" 1 time and change a to be 8.
(define-syntax while-less
    (syntax-rules (do)
        [(while-less pr do exp)
         (letrec ([n pr]
                  [aux (lambda ()
                         (if (< exp n)
                             (aux)
                             #t)) ])
              (aux))]))
