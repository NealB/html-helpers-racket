#lang webracket
(require (for-syntax racket/base racket/match racket/string racket/format threading))


(define-syntax (call! stx)
  ; calls a js method
  ; example:
  ;   (call! format-result-promise.then (procedure->external prettier-format-callback))
  (match-define (list _ dotted-name args ...) (syntax->list stx))
  (match-define (list-rest obj-tok properties) (string-split (~a (syntax->datum dotted-name)) "."))
  (define obj
    (if (string=? obj-tok "global")
        '(js-global-this)
        (string->symbol obj-tok)))
  
  (define-values (result method)
       (let loop ((props properties) (current-obj obj))
         (if (= (length props) 1)
             (values current-obj (car props))
          (loop (cdr props) `(js-ref ,current-obj ,(car props))))))
  
  (datum->syntax stx `(js-send ,result ,method  (vector ,@args))))


(define-syntax (assign! stx)
  ; assigns a value to an object property; 'global' evaluates to (js-global-this)
  ; example:
  ;   (assign! html-element-obj.innerText new-inner-text)
  ;   (assign! global.render_html_element (procedure->external render_html_element))
  (match-define (list _ dotted-name rvalue) (syntax->list stx))
  (match-define (list obj-tok properties ...) (string-split (~a (syntax->datum dotted-name)) "."))
  (define-values (obj property)
    (cond
      ((null? properties) (values '(js-global-this) obj-tok))
      ((string=? obj-tok "global") (values '(js-global-this) (car properties)))
      ((string-prefix? obj-tok "#") (values `(js-get-element-by-id ,(substring obj-tok 1)) (car properties)))
      (else (values (string->symbol obj-tok) (car properties)))))
  
  (datum->syntax stx `(js-set! ,obj ,property  ,rvalue)))

(define-syntax (get! stx)
  (match-define (list _ dotted-name) (syntax->list stx))
  (let*
      ([name-path (string-split (~a (syntax->datum dotted-name)) ".")]
       [name-path
        (if (= (length name-path) 1)
            (cons "global" name-path)
            name-path)])
    (match-define (list obj-tok properties ...) name-path)
    
    (define obj
      (cond
        [(string=? obj-tok "global") '(js-global-this)]
        [(string-prefix? obj-tok "#") `(js-get-element-by-id ,(substring obj-tok 1))]
        [else (string->symbol obj-tok)]))

    (define result
      (let loop ((props properties) (current-obj obj))
        (if (null? props)
            current-obj
            (loop (cdr props) `(js-ref ,current-obj ,(car props))))))
    (datum->syntax stx result)))

(define-syntax-rule
  (%rx func args ...)
  (js-send (get! global.rxjs) (~a (quote func)) (vector args ...)))

(define-syntax (! stx)
  (syntax-case stx ()
    [(_! obs func args body ...)
     (member (syntax-e #'func) '(map mergeMap filter tap))
     #'(js-send obs "pipe" 
                (vector (%rx func (external-lambda args body ...))))]
            
    [(_! obs func args ...)
     #'(js-send obs "pipe" (vector (%rx func args ...)))]
    [_
     (raise-syntax-error '! "bad syntax" stx)]))

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

(define-syntax-rule (ext-λ args ...)
  (external-lambda args ...))

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
    [(import-js-symbols name) (define name (js-var (~a (quote name))))]
    [(import-js-symbols name name-rest ...)
     (begin (import-js-symbols name) (import-js-symbols name-rest ...))
     ]))

(define (js-log* v)
  (define os (open-output-string))
  (write v os)
  (js-log (get-output-string os)))

(define (js-log-fmt fmt . args)
  (js-log (apply format fmt args)))

(define (js-log** msg obj)
  (js-log msg)
  (js-log* obj))

(define (js-object* . entries)
  (js-object
   (list->vector (map (λ (entry)
                        (match-define (list key value) entry)
                        (vector (format "~a" key) value))
                      entries))))



