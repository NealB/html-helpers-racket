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
        
        ;(js-log rxjs)
        ;(define ajax (get! rxjs.ajax))
        ;(define ajax (get! ajax0.ajax))
        ;(js-log ajax)
       ; (define (pipe-to obs . args)
        ;  (js-send obs "pipe" (list->vector args)))
        
        (~~>
          (%rx concat
                 (%rx from #(1))
                 (~~>
                  (%rx timer 2 1000)
                  
                  (%pipe-to "map"
                            (external-lambda (x _)
                                             (procedure-rename
                                              ((λ ()
                                                (js-log "ajax obj ->")
                                                ;(js-log (get! rxjs.ajax))
                                                (define ajax0 (get! rxjs.ajax))
                                                (js-log ajax0)
                                                ;(assign! global.rxjs_ajax ajax)
                                                 (define ajax (get! ajax0.ajax))
                                                 (js-log ajax)
                                                 ;(define result (js-send ajax "getJSON" (vector "/GetStatsAjax")))
                                                 
                                                 (define result (call! ajax0.ajax "/GetStatsAjax"))
                                                 (js-log "sent ajax")
                                                 (js-log result)
                                                 result
                                                 )) 'ajax-block)))
                  
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


 