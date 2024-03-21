#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(provide
 platform-values)

(define (platform-values)
  (values maximize-fd-limit!))

(define (maximize-fd-limit! [status void])
  (let/ec esc
    (define limits (make-RLIMIT 0 0))
    (unless (zero? (getrlimit RLIMIT_NOFILE limits))
      (status "failed to get RLIMIT_NOFILE")
      (esc))
    (unless (= (RLIMIT-rlim_cur limits)
               (RLIMIT-rlim_max limits))
      (define old-limit (RLIMIT-rlim_cur limits))
      (define new-limit (min 65000 (RLIMIT-rlim_max limits)))
      (set-RLIMIT-rlim_cur! limits new-limit)
      (status "bumping RLIMIT_NOFILE from ~a to ~a" old-limit new-limit)
      (unless (zero? (setrlimit RLIMIT_NOFILE limits))
        (status "failed to set RLIMIT_NOFILE")
        (esc)))))

(define-ffi-definer define-libc #f)

(define RLIMIT_NOFILE 8)

(define-cstruct _RLIMIT
  ([rlim_cur _uint64]
   [rlim_max _uint64]))

(define-libc getrlimit
  (_fun _int _RLIMIT-pointer -> _int))
(define-libc setrlimit
  (_fun _int _RLIMIT-pointer -> _int))
