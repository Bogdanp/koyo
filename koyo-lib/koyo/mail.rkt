#lang racket/base

(require component
         racket/contract/base
         racket/generic
         racket/hash
         racket/list
         racket/string
         "util.rkt")


;; Adapters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 gen:mail-adapter
 mail-adapter?
 mail-adapter-send-email
 mail-adapter-send-email-with-template)

(define-logger mail-adapter)

(define-generics mail-adapter
  (mail-adapter-send-email
   mail-adapter
   #:to to
   #:from from
   #:subject subject
   #:text-content [text-content]
   #:html-content [html-content])
  (mail-adapter-send-email-with-template
   mail-adapter
   #:to to
   #:from from
   #:template-id [template-id]
   #:template-alias [template-alias]
   #:template-model template-model))


;; Stub adapter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [make-stub-mail-adapter (-> mail-adapter?)]
  [stub-mail-adapter? (-> any/c boolean?)]
  [stub-mail-adapter-outbox (-> stub-mail-adapter? (listof hash?))]))

(struct stub-mail-adapter (queue)
  #:methods gen:mail-adapter
  [(define (mail-adapter-send-email ma
                                    #:to to
                                    #:from from
                                    #:subject subject
                                    #:text-content [text-content #f]
                                    #:html-content [html-content #f])
     (define message
       (hasheq 'to to
               'from from
               'subject subject
               'text-content text-content
               'html-content html-content))

     (push-message! ma message)
     (log-mail-adapter-info "email added to outbox ~v" message))

   (define (mail-adapter-send-email-with-template ma
                                                  #:to to
                                                  #:from from
                                                  #:template-id [template-id #f]
                                                  #:template-alias [template-alias #f]
                                                  #:template-model template-model)
     (define message
       (hasheq 'to to
               'from from
               'template (or template-id template-alias)
               'template-model template-model))

     (push-message! ma message)
     (log-mail-adapter-info "templated email added to outbox ~v" message))])

(define (make-stub-mail-adapter)
  (stub-mail-adapter (box null)))

(define (stub-mail-adapter-outbox ma)
  (unbox (stub-mail-adapter-queue ma)))

(define (push-message! ma m)
  (box-swap! (stub-mail-adapter-queue ma)
             (lambda (queue)
               (cons m queue))))


;; Mailer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [make-mailer-factory
   (-> #:adapter mail-adapter?
       #:sender non-empty-string?
       #:common-variables (hash/c symbol? string?)
       (-> mailer?))]
  [mailer? (-> any/c boolean?)]
  [mailer-adapter (-> mailer? mail-adapter?)]
  [mailer-sender (-> mailer? non-empty-string?)]
  [mailer-common-variables (-> mailer? (hash/c symbol? string?))]
  [mailer-merge-common-variables (-> mailer? any/c ... (hash/c symbol? string?))]
  [mailer-send-email
   (->i [(m mailer?)
         #:to [to non-empty-string?]
         #:subject [subject non-empty-string?]]
        [#:from [from non-empty-string?]
         #:text-content [text-content (or/c #f string?)]
         #:html-content [html-content (or/c #f string?)]]
        #:pre/name (text-content html-content)
        "at least one of #:text-content or #:html-content must be provided"
        (not (and (unsupplied-arg? text-content)
                  (unsupplied-arg? html-content)))
        [result void?])]
  [mailer-send-email-with-template
   (->i [[m mailer?]
         #:to [to non-empty-string?]]
        [#:from [from non-empty-string?]
         #:template-id [template-id (or/c #f exact-positive-integer?)]
         #:template-alias [template-alias (or/c #f symbol?)]
         #:template-model [template-model (hash/c symbol? string?)]]
        #:pre/name (template-id template-alias)
        "either #:template-id or #:template-alias must be provided, but not both"
        (cond
          [(unsupplied-arg? template-id) (not (unsupplied-arg? template-alias))]
          [else (not (unsupplied-arg? template-id))])
        [result void?])]))

(struct mailer (adapter sender common-variables)
  #:methods gen:component [])

(define ((make-mailer-factory #:adapter adapter
                              #:sender sender
                              #:common-variables common-variables))
  (mailer adapter sender common-variables))

(define (mailer-merge-common-variables m . variables)
  (hash-union
   (mailer-common-variables m)
   (apply hasheq variables)
   #:combine/key (lambda (_k1 _k2 v) v)))

(define (mailer-send-email m
                           #:to to
                           #:from [from (mailer-sender m)]
                           #:subject subject
                           #:text-content [text-content #f]
                           #:html-content [html-content #f])
  (mail-adapter-send-email
   (mailer-adapter m)
   #:to to
   #:from from
   #:subject subject
   #:text-content text-content
   #:html-content html-content))

(define (mailer-send-email-with-template m
                                         #:to to
                                         #:from [from (mailer-sender m)]
                                         #:template-id [template-id #f]
                                         #:template-alias [template-alias #f]
                                         #:template-model [template-model (hasheq)])
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
