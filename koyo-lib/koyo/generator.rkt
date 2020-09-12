#lang at-exp racket/base

(require racket/format
         racket/list
         racket/string)

(provide
 generate-dockerfile!)

(define (unindent s)
  (regexp-replace #px"^ +" s ""))

(define (configify s)
  (string-replace (string-upcase s) "-" "_"))

(define (generate-dockerfile! project-root project-name)
  (define (config-var id)
    (configify (format "~a-~a" project-name id)))
  (define racket-image
    (format "racket/racket:~a~a-full"
            (string-join (take (string-split (version) ".") 2) ".")
            (case (system-type 'vm)
              [(chez-scheme) "-cs"]
              [else ""])))
  (with-output-to-file (build-path project-root ".dockerignore")
    (lambda ()
      (display (unindent @~a{
                             compiled
                             node_modules}))))
  (with-output-to-file (build-path project-root "Dockerfile")
    (lambda ()
      (display (unindent @~a{
                             FROM node:12 AS assets

                             RUN mkdir -p /opt/app
                             WORKDIR /opt/app
                             COPY package.json /opt/app/package.json
                             RUN npm install

                             COPY . /opt/app
                             RUN npm run build

                             FROM @|racket-image| AS build

                             RUN mkdir -p /opt/app
                             WORKDIR /opt/app

                             COPY . /opt/app
                             RUN raco pkg install --batch --auto -D @|project-name|/

                             COPY --from=assets /opt/app/static /opt/app/static
                             RUN raco koyo dist ++lang north

                             FROM debian:10-slim

                             RUN apt-get update -y \
                               && apt-get install -y --no-install-recommends \
                                    dumb-init \
                                    libargon2-1 \
                                    openssl

                             ENV @(config-var 'http-host) "0.0.0.0"
                             COPY --from=build /opt/app/dist /opt/dist
                             CMD ["dumb-init", "/opt/dist/bin/@|project-name|"]
                             })))))
