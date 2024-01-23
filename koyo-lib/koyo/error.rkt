#lang racket/base

(require (for-syntax racket/base)
         errortrace/errortrace-key  ;; implementation detail!
         racket/contract/base
         racket/format
         racket/match
         racket/port
         racket/runtime-path
         web-server/http
         web-server/servlet/servlet-structs
         "contract.rkt"
         "haml.rkt")

(provide
 (contract-out
  [current-production-error-page (parameter/c (-> request? exn? can-be-response?))]
  [wrap-errors (-> boolean? middleware/c)]))

(define-runtime-path css-path
  (build-path "resources" "error.css"))

(define debug-styles
  (call-with-input-file css-path port->string))

(define current-production-error-page
  (make-parameter
   (lambda (_req _exn)
     (response/xexpr
      #:code 500
      (haml
       (:html
        (:head
         (:title "Internal Server Error"))
        (:body
         (:h1 "Internal Error")
         (:p "An unexpected error occurred.  Please try again later."))))))))

(define (((wrap-errors debug?) handler) req . args)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     ((error-display-handler) (exn-message e) e)
                     (if debug?
                         (render-error-page req e)
                         ((current-production-error-page) req e)))])
    (apply handler req args)))

(define (render-error-page _req e)
  (response/xexpr
   #:code 500
   (haml
    (:html
     (:head
      (:title "Internal Server Error")
      (:style debug-styles))
     (:body
      (.title
       (.container
        (:h4.title__error-label "Exception")
        (:h1.title__error-name (symbol->string (object-name e)))))

      (.container
       (render-error-message e)
       (render-stack-trace e)))))))

(define (render-error-message e)
  (haml
   (.error-message
    (:pre (exn-message e)))))

(define (render-stack-trace e)
  (haml
   (:ul.stack-trace
    ,@(remove-repeats
       (for/list ([frame (in-list (continuation-mark-set->list (exn-continuation-marks e) errortrace-key))]
                  #:unless (null? frame))
         (render-errortrace-frame frame)))
    ,@(remove-repeats
       (for/list ([frame (in-list (continuation-mark-set->context (exn-continuation-marks e)))])
         (render-stack-frame frame))))))

(define (render-errortrace-frame frame)
  (match-define (list stx source line _column _position _span) frame)
  (render-frame stx source line))

(define (render-stack-frame frame)
  (match-define (cons name location) frame)
  (if location
      (render-frame name (srcloc-source location) (srcloc-line location))
      (render-frame name #f #f)))

(define (render-frame expr source line)
  (haml
   (:li
    (:code.frame-function (if expr
                              (~a expr)
                              "<anonymous function>"))
    (.frame-location
     " in " (:code (->string/unknown source))
     " at line " (:code (->string/unknown line))))))

(define (remove-repeats items)
  (for/lists (seen)
             ([item (in-list items)]
              #:when (or (null? seen)
                         (not (equal? item (car seen)))))
    item))

(define (->string/unknown v)
  (cond
    [v => ~a]
    [else "<unknown>"]))
