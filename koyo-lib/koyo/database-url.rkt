#lang racket/base

(require db
         net/url
         racket/contract
         racket/match
         racket/string)

(provide
 parse-database-url)

(define/contract (parse-database-url s)
  (-> non-empty-string?
      (values procedure?
              (or/c false/c non-empty-string?)
              (or/c false/c (integer-in 1 65535))
              (or/c non-empty-string? 'memory 'temporary)
              (or/c false/c string?)
              (or/c false/c string?)))

  (define u (string->url s))
  (define-values (dbsystem connector)
    (scheme->dbystem&connector (url-scheme u)))

  (define host
    (case (url-host u)
      [("") "127.0.0.1"]
      [else (url-host u)]))

  (define port
    (or (url-port u)
        (case dbsystem
          [(mysql) 3306]
          [(postgresql) 5432]
          [(sqlite3) #f]
          [else (error 'parse-database-url "unknown db system")])))

  (define database
    (string-join (map path/param-path (url-path u)) "/"))
  (when (string=? database "")
    (error 'parse-database-url "database not provided"))

  (define-values (username password)
    (match (regexp-match #rx"^([^:]+)(:(.+))?$" (or (url-user u) ""))
      [#f
       (values #f #f)]

      [(list _ username _ password)
       (values username password)]))

  (values
   connector
   (case dbsystem
     [(sqlite3) #f]
     [else host])
   (case dbsystem
     [(sqlite3) #f]
     [else port])
   (case dbsystem
     [(sqlite3)
      (case database
        [(":memory:") 'memory]
        [(":temporary:") 'temporary]
        [else database])]
     [else database])
   username
   password))

(define (scheme->dbystem&connector s)
  (case s
    [("mysql" "mariadb")
     (values 'mysql mysql-connect)]

    [("postgres" "postgresql")
     (values 'postgresql postgresql-connect)]

    [("sqlite" "sqlite3")
     (values 'sqlite3 sqlite3-connect)]

    [else
     (error 'parse-database-url "unsupported database")]))
