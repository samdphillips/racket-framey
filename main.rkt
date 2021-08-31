#lang racket/base

(require racket/generic
         (for-syntax racket/base)
         syntax/parse/define)

(provide df-select
         in-data-frame
         (rename-out
          [df-graphite? data-frame?]))

(define (->symbol s)
  (cond
    [(symbol? s) s]
    [(string? s) (string->symbol s)]))

(define-syntax-parse-rule (define-dynamic name:id
                            {~optional import-name
                                       #:defaults ([import-name #'name])}
                            modname:id
                            missing)
  (define name (dynamic-require 'modname 'import-name (lambda () missing))))

(define-dynamic table? tabular-asa (lambda (x) #f))
(define-dynamic data-frame? data-frame (lambda (x) #f))

(define-generics df-graphite
  (df-select df-graphite name)
  (in-data-frame df-graphite name)
  #:defaults
  ([table?
    (define-dynamic table-column tabular-asa (lambda xs (error 'df-graphite "missing function: table-column")))
    (define (df-select df name)
      (define name-sym (->symbol name))
      (for/vector ([v (table-column df name-sym)]) v))
    (define (in-data-frame df name)
      (define name-sym (->symbol name))
      (table-column df name-sym))]
   [data-frame?
    (define-dynamic
      df-select^ df-select data-frame
      (lambda xs (error 'df-graphite "missing function: df-select")))
    (define-dynamic
      in-data-frame^ in-data-frame data-frame
      (lambda xs (error 'df-graphite "missing function: in-data-frame")))
    (define df-select df-select^)
    (define in-data-frame in-data-frame^)]))

