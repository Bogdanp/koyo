#lang racket/base

(require racket/contract
         web-server/http)

;; Requests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 bindings-ref
 bindings-ref-bytes
 bindings-ref-number
 bindings-ref-symbol)

(define/contract (bindings-ref bindings name [default #f])
  (->* ((listof binding?) symbol?) ((or/c false/c string?)) (or/c false/c string?))
  (cond
    [(bindings-assq (string->bytes/utf-8 (symbol->string name)) bindings)
     => (compose1 bytes->string/utf-8 binding:form-value)]

    [else default]))

(define ((make-bindings-reffer p) bindings name [default #f])
  (cond
    [(bindings-ref bindings name) => p]
    [else default]))

(define bindings-ref-bytes  (make-bindings-reffer string->bytes/utf-8))
(define bindings-ref-number (make-bindings-reffer string->number))
(define bindings-ref-symbol (make-bindings-reffer string->symbol))
