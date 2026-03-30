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


(on-do! (html# rxjs) load _
        (js-log "rxjs loaded")
           
        (import-js-symbols rxjs)

        (define-syntax-rule
          (%rx func args ...)
          (js-send rxjs (~a (quote func)) (vector args ...)))

        (define-syntax-rule
          (%pipe-to obs func args ...)
          (js-send obs "pipe" (vector (%rx func args ...)))) 
        
        
        (~~>
          (%rx concat
                 (%rx from #(1))
                 (~~>
                  (%rx timer 2 1000)
                  
                  (%pipe-to "map"
                            (external-lambda (x _)
                                             (js-log "in first map")
                                             ;(js-log "got global; will call fetch")
                                             (define prom (call! global.fetch "http://neal2500k:8080/GetStatsAjax"))
                                             ;(define prom (call! global.fromFetch "http://neal2500k:8080/GetStatsAjax"))
                                             (js-log "sent ajax")
                                             ;(call! rxjs.from prom)
                                             (call! rxjs.from prom)
                                             ))

                  ;(%pipe-to "switchMap"
                  ;          (external-lambda (x _)
                  ;                           (js-log "in switch map")
                  ;                           (js-log x)
                  ;                           (call! x.json)))

                  (%pipe-to "mergeAll")

                  (%pipe-to "map"
                            (external-lambda (x _)
                                             (js-log "map got... x ->")
                                             (js-log x)
                                             (define y (call! x.json))
                                             (js-log "y =")
                                             (js-log y)
                                             (call! rxjs.from y)))
                  
                  (%pipe-to "mergeAll")

                  (%pipe-to "map"
                            (external-lambda (z _)
                                             (js-log "map got... z ->")
                                             (js-log z)

                                             (define con (get! global.Observable))
                                             (js-new con (vector
                                                          (external-lambda (subscriber)
                                                                           )

                                             z))
                                    
                 #|  #;(==> (%rx map (external-lambda (x _)
                                                      (js-log "ajax obj ->")
                                                      (js-log (get! rxjs.ajax))
                                                      (define ajax (get! rxjs.ajax))
                                                      (call! ajax.getJSON "/GetStatsAjax")
                                                      )))
                   |#
                  
                  
                 
                  ))
   
          (js-send "subscribe" (vector
                                (external-lambda (x)
                                                 (js-log (format "in subscribe again. external-lambda: ~a" x))
                                                 ;(js-log x)
                                                 ))))
        (void))


 