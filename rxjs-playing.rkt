#lang webracket


(define rxjs-script
  (renderHtmlElement '(script .#rxjs (src "https://unpkg.com/rxjs@%5E7/dist/bundles/rxjs.umd.min.js"))))

(js-append-child! (js-document-head) (sxml->dom (car rxjs-script)))


;#;(define-syntax-rule (%rx func args ...)
;    (js-send rxjs (~a (quote func)) (vector args ...)))


;(define-syntax (pipe stx)
;  (match-define (list _ source operators ...) (syntax->list stx))
  ;(match-define (list obj method) (string-split (~a (syntax->datum dotted-name)) "."))
  
;  (datum->syntax stx `(js-send ,(string->symbol obj) ,method  (vector ,@args))))


;(define-syntax-rule (pipe~> init funcs ...)
;  (~~>
;   init
   
  ;(js-send rxjs (~a (quote func)) (vector args ...)))

(define (js-log* v)
  (define os (open-output-string))
  (write v os)
  (js-log (get-output-string os)))

(define (js-log-fmt fmt . args)
  (js-log (apply format fmt args)))

(define (js-object* . entries)
  (js-object
   (list->vector (map list->vector entries))))

(on-do! (html# rxjs) load _
        (js-log "rxjs loaded")
           
        (import-js-symbols rxjs Object)

        (define-syntax-rule
          (%rx func args ...)
          (js-send rxjs (~a (quote func)) (vector args ...)))

        (define-syntax-rule
          (! obs func args ...)
          (js-send obs "pipe" (vector (%rx func args ...)))) 
        
        
        (~~>
         (%rx concat
              (%rx from #(1))
              (~~>
               (%rx timer 2 1000)
                  
               (! mergeMap
                  (external-lambda (x _)

                                   ;(js-log "sending ajax")

                                   (call! rxjs.ajax.ajax (js-object
                                                          #(#("url" "http://neal2500k:8080/GetStatsAjax")
                                                            #("crossDomain" #t))))
                                   ))


               (! map
                  (external-lambda (x _)
                                   (define y (get! x.response))
                                   ;(js-log "y =")
                                   ;(js-log y)
                                   y
                                   ))

               (! pairwise)

               (! map
                  (external-lambda (pairArray _)
                                   (define old (vector-ref pairArray 0))
                                   (define new (vector-ref pairArray 1))

                                   (define oldVec (call! Object.entries old))
                                   (define oldAssoc (for/list ((a (in-vector oldVec)))
                                                      (let* ((l (vector->list a)))
                                                        (apply cons l))))

                                   (define newVec (call! Object.entries new))
                                   (define newAssoc (for/list ((a (in-vector newVec)))
                                                      (let* ((l (vector->list a)))
                                                        (apply cons l))))
                                   
                                   (for/list ((key-value (in-list newAssoc))
                                              #:do ((match-define (cons key-new value-new) key-value))
                                              #:do ((match-define (cons _       value-old) (assoc key-new oldAssoc)))
                                              #:when (> value-new value-old))
                                     
                                     ;(js-log-fmt "~s -> ~s (diff = ~s)" value-old value-new (- value-new value-old))

                                     (list value-old value-new (- value-new value-old)))))

               (! filter
                  (external-lambda (diff-list _)
                                   ;(js-log* diff-list)
                                   (define result (not (null? diff-list)))
                                   (when result (js-log-fmt "(not (null? diff-list)) = ~a" result))
                                   result
                                   ))
               ))
   
         (js-send "subscribe" (vector
                               (js-object*
                                `("next" 
                                  ,(external-lambda (x)
                                                    (js-log (format "in subscribe again. external-lambda x: ~a" (js-value->string x)))
                                                    (js-log x)
                                                    (js-log "done showing value")
                                                    )))))))
(void))


