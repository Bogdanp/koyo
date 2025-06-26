#lang racket/base

(require db
         db/util/postgresql
         racket/contract/base
         racket/match
         racket/port
         racket/string)

(provide
 insert-batcher?
 (contract-out
  [make-insert-batcher
   (->* [symbol? (listof (list/c symbol? string?))]
        [#:alias (or/c #f symbol?)
         #:dialect (or/c 'postgresql)
         #:batch-size exact-positive-integer?
         #:on-conflict (or/c 'error
                             (list/c 'do-nothing (listof symbol?))
                             (list/c 'update (listof symbol?) (listof (list/c symbol? string?))))]
        insert-batcher?)]
  [ib-push!
   (-> insert-batcher? connection? any/c any/c ... void?)]
  [ib-flush!
   (-> insert-batcher? connection? void?)]))

(struct insert-batcher
  (table
   columns
   dialect
   batch-size
   on-conflict
   statement
   [data #:mutable]
   [n-cols #:mutable]
   [n-rows #:mutable]))

(define (make-insert-batcher
         table columns
         #:alias [alias #f]
         #:dialect [dialect 'postgresql]
         #:batch-size [batch-size 10000]
         #:on-conflict [on-conflict 'error])
  (define stmt
    (virtual-statement
     (case dialect
       [(postgresql) (make-insert-batcher-stmt/pg table alias columns on-conflict)]
       [else (error 'make-insert-batcher "not implemented for dialect ~s" dialect)])))
  (insert-batcher
   #;table table
   #;columns columns
   #;dialect dialect
   #;batch-size batch-size
   #;on-conflict on-conflict
   #;statement stmt
   #;data (make-vector (length columns) null)
   #;n-cols (length columns)
   #;n-rows 0))

(define (make-insert-batcher-stmt/pg table alias columns on-conflict)
  (call-with-output-string
   (lambda (out)
     (define columns-str
       (string-join
        (for/list ([column (in-list columns)])
          (symbol->string (car column)))
        ", "))
     (define placeholders-str
       (string-join
        (for/list ([(column idx) (in-indexed (in-list columns))])
          (match-define (list _ type) column)
          (format "$~a::~a[]" (add1 idx) type))
        ", "))
     (if alias
         (fprintf out "INSERT INTO ~a AS ~a (~n" table alias)
         (fprintf out "INSERT INTO ~a (~n" table))
     (fprintf out "  ~a~n" columns-str)
     (fprintf out ") SELECT * FROM UNNEST(~a) AS t(~a)~n" placeholders-str columns-str)
     (match on-conflict
       ['error (void)]
       [`(do-nothing ,conflict-columns)
        (define conflict-columns-str (string-join (map symbol->string conflict-columns) ", "))
        (fprintf out "ON CONFLICT (~a) DO NOTHING" conflict-columns-str)]
       [`(update ,conflict-columns ,updates)
        (define conflict-columns-str (string-join (map symbol->string conflict-columns) ", "))
        (fprintf out "ON CONFLICT (~a) DO UPDATE SET~n" conflict-columns-str)
        (for ([(col idx) (in-indexed (in-list updates))])
          (match-define (list column expr) col)
          (if (zero? idx)
              (fprintf out "  ")
              (fprintf out " ,"))
          (fprintf out "~a = ~a~n" column expr))]))))

(define (insert-batcher-exec ib conn)
  (define dialect (dbsystem-name (connection-dbsystem conn)))
  (unless (eq? dialect (insert-batcher-dialect ib))
    (define message (format "(equal/c ~s (dialect conn))" (insert-batcher-dialect ib)))
    (raise-argument-error 'insert-batcher-exec message conn))
  (case dialect
    [(postgresql) (insert-batcher-exec/pg ib conn)]
    [else (error 'insert-batcher-exec "not implemented for dialect ~s" dialect)]))

(define (insert-batcher-exec/pg ib conn)
  (match-define (insert-batcher _ _ _ _ _ stmt data _ _) ib)
  (define args
    (for/list ([col-values (in-vector data)])
      (list->pg-array (reverse col-values))))
  (apply query-exec conn stmt args))

(define (ib-push! ib conn . cols)
  (match-define (insert-batcher _ _ _ batch-size _ _ data n-cols n-rows) ib)
  (unless (= (length cols) n-cols)
    (define arity (arity-at-least (+ 2 n-cols)))
    (append raise-arity-error 'ib-push! arity ib conn cols))
  (cond
    [(= n-rows batch-size)
     (ib-flush! ib conn)
     (apply ib-push! ib conn cols)]
    [else
     (for ([(col idx) (in-indexed (in-list cols))])
       (vector-set! data idx (cons col (vector-ref data idx))))
     (set-insert-batcher-n-rows! ib (add1 n-rows))]))

(define (ib-flush! ib conn)
  (match-define (insert-batcher _ _ _ _ _ _ data _ n-rows) ib)
  (unless (zero? n-rows)
    (insert-batcher-exec ib conn)
    (set-insert-batcher-n-rows! ib 0)
    (for ([idx (in-range (vector-length data))])
      (vector-set! data idx null))))
