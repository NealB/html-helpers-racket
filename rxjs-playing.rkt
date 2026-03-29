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

        (define-syntax-rule (%rx func args ...)
          (js-send rxjs (~a (quote func)) (vector args ...)))
        
        (js-log rxjs)
   
        (define (==> obs . args)
          (js-send obs "pipe" (list->vector args)))
        
        (~~>
          (%rx concat
                 (%rx from #(1))
                 (~~>
                  (%rx timer 2 1000)
                  
                  (==> (%rx map (external-lambda (x _)
                                                 (js-log "ajax obj ->")
                                                 (js-log (get! global.ajax))
                                                 (define ajax_ (get! global.ajax))
                                                 (call! ajax_.getJSON "/GetStatsAjax")
                                                 )))
                  
                  #;(==> (%rx map (external-lambda (x _)
                                                   (js-log x)
                                                   x)))
                  
                  
                 
                  ))
   
          (js-send "subscribe" (vector
                                (external-lambda (x)
                                                 (js-log (format "in subscribe again. external-lambda: ~a" x))
                                                 ;(js-log x)
                                                 ))))
        (void))


 