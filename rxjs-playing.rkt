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

                                   (js-log "sending ajax")

                                   (call! rxjs.ajax.ajax (js-object
                                                          #(#("url" "http://neal2500k:8080/GetStatsAjax")
                                                            #("crossDomain" #t))))
                                   ))


               (! map
                  (external-lambda (x _)
                                   (define y (get! x.response))
                                   (js-log "y =")
                                   (js-log y)
                                   y
                                   ))

               #;(! map
                  (external-lambda (z _)
                                   (js-log "map got... z ->")
                                   (js-log z)

                                   (define milky (js-ref z "http://milkyway.cs.rpi.edu/milkyway/"))
                                   (js-log "milky:")
                                   (js-log milky)

                                   
                                   (call! Object.keys z)
                                   
                                   ))

               (! pairwise)


               ;(match-define (list-rest obj-tok properties) (string-split (~a (syntax->datum dotted-name)) "."))
               (! concatMap
                  (external-lambda (pairArray _)
                                   (js-log "old, new ->")

                                   (define old (vector-ref pairArray 0))
                                   (define new (vector-ref pairArray 1))

                                   (js-log old)
                                   (js-log new)

                                   (define oldVec (call! Object.entries old))
                                   (define oldAssoc (for/list ((a (in-vector oldVec)))
                                                      (let* ((l (vector->list a))
                                                             (c (apply cons l)))
                                                        c)))
                                   (define oldHash (make-hash oldAssoc))
                                   (js-log "oldHash")
                                   (js-log* oldHash)
                  
                                   (define newVec (call! Object.entries new))
                                   (define newAssoc (for/list ((a (in-vector newVec)))
                                                      (let* ((l (vector->list a))
                                                             (c (apply cons l)))
                                                        c)))
                                   (define newHash (make-hash newAssoc))
                                   (js-log "newHash")
                                   (js-log* newHash)
                  
                                   (define differences
                                     (hash-filter newHash (λ (key value)
                                                            (not
                                                             (and
                                                              (hash-has-key? oldHash key)
                                                              (= value (hash-ref oldHash key)))))))

                                   (js-log "differences")
                                   (js-log* differences)
                                   
                                   ;(define x
                                   ;  (match x
                                   ;    ((vector
                                   ;      (
                                  ; 
                                   ))
               
                                   ;(define keys (call! global.Object.keys pairs
               ;(! mergeAll)    
               #|  #;(==> (%rx map (external-lambda (x _)
                                                      (js-log "ajax obj ->")
                                                      (js-log (get! rxjs.ajax))
                                                      (define ajax (get! rxjs.ajax))
                                                      (call! ajax.getJSON "/GetStatsAjax")
                                                      )))
                   |#


               ))
   
         (js-send "subscribe" (vector
                               (js-object
                                (vector
                                 (vector "next" 
                                         (external-lambda (x)
                                                          (js-log (format "in subscribe again. external-lambda x: ~a" (js-value->string x)))
                                                          (js-log x)
                                                          (js-log "done showing value")
                                                          )))))))
        (void))


