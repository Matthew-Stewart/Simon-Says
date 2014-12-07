#lang racket

(require rackunit)

(provide write-to-string
         read-from-string
         string->struct/maker
         vectors->structs)

;; writes a value into a string
(define (write-to-string val)
  (with-output-to-string 
   (lambda () (write val))))

;; reads a value from a string
(define (read-from-string str)
  (with-input-from-string str read))

;; turn strings into structures recursively
;; (listof (list/c symbol maker)) string -> any
(define (string->struct/maker maker-table string)
  (define vectorized (read-from-string string))
  (vectors->structs maker-table vectorized))

;; find all vectors, turn them into the corresponding structures
(define (vectors->structs table data)
  (cond
    [(vector? data) 
     (unless (<= 1 (vector-length data))
       (raise-argument-error 'vector->structs "vector of length 1 or more"
                             1 table data))
     (define label (vector-ref data 0))
     (match (regexp-match #px"^struct:(.*)$" (symbol->string label))
       [(list _ name)
        (match (assoc name table)
          [#f (raise-argument-error 'vectors->structs
                                    "struct with name appearing in table"
                                    1 table data)]
          [(list dc maker) (apply maker (map (lambda (data)
                                               (vectors->structs table data)) 
                                             (rest (vector->list data))))])])]
    [(list? data)
     (map (lambda (data) (vectors->structs table data)) data)]
    [else data]))

(check-equal? (write-to-string (list 3 4 5))
              "(3 4 5)")

(check-equal? (read-from-string "(3 4 5)")
              (list 3 4 5))

(define-struct jar (la di) #:transparent)

(define my-table (list (list "jar" make-jar)))

(check-equal? (string->struct/maker my-table
                                    (write-to-string (make-jar "abc" 34)))
              (make-jar "abc" 34))

(check-equal? (string->struct/maker my-table
                                    (write-to-string (make-jar "abc" 
                                                               (make-jar
                                                                3 3))))
               (make-jar "abc" (make-jar 3 3)))

(check-equal? (string->struct/maker my-table
                                    (write-to-string (make-jar "abc" 
                                                               (list
                                                                (make-jar
                                                                 3 3)))))
               (make-jar "abc" (list (make-jar 3 3))))
