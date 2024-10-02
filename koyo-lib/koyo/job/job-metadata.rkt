#lang racket/base

(provide
 (struct-out job-metadata))

(struct job-metadata (id queue name attempts)
  #:transparent)
