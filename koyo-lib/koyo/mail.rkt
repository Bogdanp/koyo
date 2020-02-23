#lang racket/base

(require component
         racket/contract
         racket/function
         racket/generic
         racket/hash
         racket/list
         racket/string
         "util.rkt")


;; Adapters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 gen:mail-adapter
 mail-adapter?
 mail-adapter-send-email-with-template)

(define-logger mail-adapter)

(define-generics mail-adapter
  (mail-adapter-send-email-with-template
   mail-adapter
   #:to to
   #:from from
   #:template-id [template-id]
   #:template-alias [template-alias]
   #:template-model template-model))


;; Stub adapter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-stub-mail-adapter
 stub-mail-adapter?
 stub-mail-adapter-outbox)

(struct stub-mail-adapter (queue)
  #:methods gen:mail-adapter
  [(define (mail-adapter-send-email-with-template ma
                                                  #:to to
                                                  #:from from
                                                  #:template-id [template-id #f]
                                                  #:template-alias [template-alias #f]
                                                  #:template-model template-model)
     (unless (or template-id template-alias)
       (raise-user-error 'mail-adapter-send-email-with-template
                         "either template-id or template-alias must be provided"))

     (define message
       (hasheq 'to to
               'from from
               'template (or template-id template-alias)
               'template-model template-model))

     (box-swap! (stub-mail-adapter-queue ma) (curry cons message))
     (log-mail-adapter-info "templated email added to outbox ~v" message))])

(define/contract (make-stub-mail-adapter)
  (-> mail-adapter?)
  (stub-mail-adapter (box null)))

(define/contract (stub-mail-adapter-outbox ma)
  (-> stub-mail-adapter? (listof hash?))
  (unbox (stub-mail-adapter-queue ma)))


;; Mailer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 make-mailer-factory
 mailer?
 mailer-adapter
 mailer-sender
 mailer-common-variables
 mailer-merge-common-variables
 mailer-send-email-with-template)

(struct mailer (adapter sender common-variables)
  #:methods gen:component
  [(define component-start values)
   (define component-stop values)])

(define/contract ((make-mailer-factory #:adapter adapter
                                       #:sender sender
                                       #:common-variables common-variables))
  (-> #:adapter mail-adapter?
      #:sender non-empty-string?
      #:common-variables (hash/c symbol? string?)
      (-> mailer?))
  (mailer adapter sender common-variables))

(define/contract (mailer-merge-common-variables m . variables)
  (-> mailer? any/c ... (hash/c symbol? string?))
  (hash-union
   (mailer-common-variables m)
   (apply hasheq variables)
   #:combine/key (lambda (k _ v) v)))

(define/contract (mailer-send-email-with-template m
                                                  #:to to
                                                  #:from [from (mailer-sender m)]
                                                  #:template-id [template-id #f]
                                                  #:template-alias [template-alias #f]
                                                  #:template-model [template-model (hasheq)])
  (->* (mailer?
        #:to non-empty-string?)
       (#:from non-empty-string?
        #:template-id (or/c false/c exact-positive-integer?)
        #:template-alias (or/c false/c symbol?)
        #:template-model (hash/c symbol? string?))
       void?)
  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to to
   #:from from
   #:template-id template-id
   #:template-alias template-alias
   #:template-model (apply mailer-merge-common-variables m (flatten
                                                            (for/fold ([items null])
                                                                      ([(k v) (in-hash template-model)])
                                                              (cons k (cons v items)))))))
