#lang racket/base

(require (for-syntax racket/base)
         db
         racket/file
         racket/runtime-path
         "../database.rkt")

(provide
 ensure-latest-schema!)

(define-runtime-path migrations-path
  (build-path "migrations"))

(define (ensure-latest-schema! database)
  (with-database-transaction [conn database]
    (query-exec conn "CREATE TABLE IF NOT EXISTS koyo_job_schema_version(version TEXT)")
    (define last-version
      (query-maybe-value conn "SELECT version FROM koyo_job_schema_version"))

    (for ([migration (sort (map path->string (directory-list migrations-path)) string-ci<?)]
          #:unless (and last-version (string-ci<=? migration last-version)))
      (query-exec conn (file->string (build-path migrations-path migration)))
      (query-exec conn "DELETE FROM koyo_job_schema_version")
      (query-exec conn "INSERT INTO koyo_job_schema_version(version) VALUES($1)" migration))))
