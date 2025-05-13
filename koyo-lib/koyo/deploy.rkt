#lang racket/base

(require racket/date
         racket/match
         racket/port
         racket/string
         racket/system
         web-server/templates)

(provide deploy)

(define current-ssh-flags
  (make-parameter
   '("-T"
     "-o" "StrictHostKeyChecking=accept-new")))

(define (deploy
         #:app-name app-name
         #:destination [destination (format "/opt/~a" app-name)]
         #:environment [environment null] ;; noqa
         #:group [group "www-data"] ;; noqa
         #:health-check? [health-check? #f] ;; noqa
         #:ports [ports (hash "blue" 8001 "green" 8002)] ;; noqa
         #:post-script [post-script #f]
         #:pre-script [pre-script #f]
         #:ssh-flags [ssh-flags null]
         #:status [status status]
         #:user [user app-name] ;; noqa
         distribution version-str hosts)
  (parameterize ([date-display-format 'iso-8601])
    (let ([distribution (path->directory-path distribution)]
          [destination (path->directory-path destination)]
          [version-str (format "~a_~a" (~version-timestamp) version-str)])
      (unless rsync (error 'deploy "rsync executable not found in PATH"))
      (unless ssh (error 'deploy "ssh executable not found in PATH"))
      (for ([str (in-list (list app-name version-str))]
            [label (in-list '("APP_NAME" "VERSION"))])
        (unless (regexp-match? #rx"^[A-Za-z0-9_-]+$" str)
          (error 'deploy "~a may only contain alphanumeric characters, dashes and underscores" label)))
      (unless (absolute-path? destination)
        (error 'deploy "DESTINATION is not an absolute path"))
      (define all-flags
        (append (current-ssh-flags) ssh-flags))
      (status "SSH Flags: ~a" all-flags)
      (parameterize ([current-ssh-flags all-flags])
        (define versions-path (build-path destination "versions"))
        (define version-path (build-path versions-path version-str))
        (define service-name (format "~a@.service" app-name))
        (status "Templating ~a..." service-name)
        (define app@.service (include-template "deploy/00-service-at.tpl"))
        (status "Templating environment file...")
        (define environment.txt (include-template "deploy/01-environment.tpl"))
        (status "Performing preflight checks...")
        (for ([host (in-list hosts)])
          (status "[~a] Testing connection..." host)
          (unless (equal? (ssh* host "echo OK") "OK")
            (error 'deploy "failed to connect to host ~s" host))
          (status "[~a] Connection OK" host))
        (status "Deploying...")
        (for ([host (in-list hosts)])
          (status "[~a] Creating versions directory..." host)
          (ssh* host (format "mkdir -p ~a/versions" destination))
          (status "[~a] Copying dist..." host)
          (rsync* "-avz" distribution (format "~a:~a" host version-path))
          (status "[~a] Determining variant..." host)
          (define variant-path (format "~a/variant" destination))
          (define current-variant
            (ssh* host (format "cat ~a 2>/dev/null || echo green" variant-path)))
          (status "[~a] == current: ~a" host current-variant)
          (define target-variant
            (case current-variant
              [("blue") "green"]
              [("green") "blue"]
              [else (error 'deploy "invalid variant: ~a" current-variant)]))
          (status "[~a] == target: ~a" host target-variant)
          (status "[~a] Linking dist..." host)
          (ssh* host (include-template "deploy/10-link-distribution.sh.tpl"))
          (status "[~a] Writing environment file..." host)
          (ssh* #:stdin (open-input-string environment.txt)
                host (include-template "deploy/11-prepare-environment.sh.tpl"))
          (status "[~a] Installing ~a..." host service-name)
          (ssh* #:stdin (open-input-string app@.service)
                host (include-template "deploy/20-install-service.sh.tpl") )
          (define run-script.sh (include-template "deploy/21-run-script.sh.tpl"))
          (when pre-script
            (status "[~a] Executing pre script..." host)
            (ssh* host run-script.sh #:stdin (open-input-string pre-script)))
          (define old-svc (format "~a@~a.service" app-name current-variant))
          (define new-svc (format "~a@~a.service" app-name target-variant))
          (status "[~a] Replacing '~a' with '~a'..." host old-svc new-svc)
          (ssh* host (include-template "deploy/22-replace-service.sh.tpl"))
          (when post-script
            (status "[~a] Executing post script..." host)
            (ssh* host run-script.sh #:stdin (open-input-string post-script)))
          (status "[~a] Deleting old deployments..." host)
          (ssh* host (include-template "deploy/30-garbage-collect.sh.tpl")))))))

(define (status message . args)
  (printf
   "[~a] ~a~n"
   (~timestamp)
   (apply format message args)))

(define (~timestamp)
  (date->string (current-date) #t))

(define (~version-timestamp)
  (regexp-replace* #rx"[-:]" (~timestamp) "_"))

(define rsync (find-executable-path "rsync"))
(define ssh (find-executable-path "ssh"))

(define (rsync* . args)
  (parameterize ([current-subprocess-custodian-mode 'kill]
                 [subprocess-group-enabled #t])
    (define program (format "~a ~a" ssh (string-join (current-ssh-flags))))
    (unless (zero? (apply system*/exit-code rsync "-e" program args))
      (error 'rsync* "command failed"))))

(define (ssh* #:stdin [in #f] . args)
  (parameterize ([current-subprocess-custodian-mode 'kill]
                 [subprocess-group-enabled #t])
    (define flags+args (append (current-ssh-flags) args))
    (match-define (list stdout stdin _pid stderr control)
      (apply process* ssh flags+args))
    (define stdout-buf (open-output-string))
    (define stderr-buf (dup-output-port (current-error-port)))
    (define pump-thds
      (for/list ([in (in-list (list stdout stderr in))]
                 [out (in-list (list stdout-buf stderr-buf stdin))])
        (thread
         (lambda ()
           (when in
             (copy-port in out)
             (close-input-port in))
           (close-output-port out)))))
    (control 'wait)
    (for-each thread-wait pump-thds)
    (unless (zero? (control 'exit-code))
      (error 'ssh* "command failed"))
    (string-trim (get-output-string stdout-buf))))
