;; This file was created by make-log-based-eval
((require component db gregor koyo)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-system
  example
  (broker (db) make-broker)
  (db
   (make-database-factory
    (lambda () (postgresql-connect #:user "example" #:database "example"))))
  (worker (broker) (make-worker-factory)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((system-start example-system) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((current-broker (system-ref example-system 'broker))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define executed? (make-semaphore))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define-job
  (say-hello name)
  (printf "hi ~a!~n" name)
  (semaphore-post executed?))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((say-hello "Bogdan") ((3) 0 () 0 () () (q values 27)) #"" #"")
((void (sync executed?))
 ((3) 0 () 0 () () (c values c (void)))
 #"hi Bogdan!\n"
 #"")
((schedule-at (+minutes (now/moment) 5) (say-hello "Bogdan"))
 ((3) 0 () 0 () () (q values 28))
 #""
 #"")
