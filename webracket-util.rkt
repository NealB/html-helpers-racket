#lang webracket
(require (for-syntax racket/base racket/match racket/string racket/format))


(define-syntax (call! stx)
  ; calls a js method
  ; example:
  ;   (call! format-result-promise.then (procedure->external prettier-format-callback))
  (match-define (list _ dotted-name args ...) (syntax->list stx))
  (match-define (list obj method) (string-split (~a (syntax->datum dotted-name)) "."))
  
  (datum->syntax stx `(js-send ,(string->symbol obj) ,method  (vector ,@args))))


(define-syntax (assign! stx)
  ; assigns a value to an object property; 'global' evaluates to (js-global-this)
  ; example:
  ;   (assign! html-element-obj.innerText new-inner-text)
  ;   (assign! global.render_html_element (procedure->external render_html_element))
  (match-define (list _ dotted-name rvalue) (syntax->list stx))
  (match-define (list obj-tok property) (string-split (~a (syntax->datum dotted-name)) "."))
  (define obj
    (if (string=? obj-tok "global")
        '(js-global-this)
        (string->symbol obj-tok)))
  
  (datum->syntax stx `(js-set! ,obj ,property  ,rvalue)))

(define-syntax (get! stx)
  (match-define (list _ dotted-name) (syntax->list stx))
  (match-define (list obj-tok property) (string-split (~a (syntax->datum dotted-name)) "."))
  (define obj
    (if (string=? obj-tok "global")
        '(js-global-this)
        (string->symbol obj-tok)))
  
  (datum->syntax stx `(js-ref! ,obj ,property)))

;(define-syntax (html# stx)
  ; takes an argument id and returns the html element with that id
;  (match-define (list _ id) (syntax->list stx))
;  (datum->syntax stx `(js-get-element-by-id ,(~a (syntax->datum id)))))

(define-syntax-rule (html# id)
  (js-get-element-by-id (~a (quote id))))

(define-syntax-rule (define-proc-external name proc-expr)
  (define name (procedure->external proc-expr)))

(define-syntax-rule (external-lambda arg-list body ...)
  (procedure->external (lambda arg-list body ...)))

(define-syntax-rule (on! toks ...)
  (js-add-event-listener! toks ...))


(define-syntax-rule (on-do! element event-name event-arg body ...)
  ; executes body expressions when element receives event event-name
  ; example:
  ;   (on-do! (html# prettier1) load e (js-log "loaded!"))
  (js-add-event-listener! element (~a (quote event-name)) (procedure->external (lambda (event-arg) body ...))))


(define-syntax import-js-symbols
  ; imports symbols from js into the current scope
  ; example:
  ;   (import-js-symbols prettier prettierPlugins)
  (syntax-rules ()
    [(import-js-symbols name) (define name (js-ref/extern (js-global-this) (~a (quote name))))]
    [(import-js-symbols name name-rest ...)
     (begin (import-js-symbols name) (import-js-symbols name-rest ...))
     ]))



