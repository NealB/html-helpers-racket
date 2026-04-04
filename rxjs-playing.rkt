#lang webracket


(define rxjs-script
  (renderHtmlElement '(script .#rxjs (src "https://unpkg.com/rxjs@%5E7/dist/bundles/rxjs.umd.min.js"))))

(js-append-child! (js-document-head) (sxml->dom (car rxjs-script)))




(on-do! (html# rxjs) load _
        (js-log "rxjs loaded")
           
        (import-js-symbols rxjs Object)

        (define (wrap-with-subject obs)
          (define Subject (get! rxjs.Subject))
          (define ajax-subject (js-new Subject (vector)))
        
          (call! obs.subscribe ajax-subject)

          ajax-subject)

        (let* ((ajax-ticks-obs

                (wrap-with-subject
                 (%rx concat
                      (%rx from #(1))
                      (~~>
                       (%rx timer 2 1000)
                  
                       (! mergeMap (x _)
                          (call! rxjs.ajax.ajax (js-object*
                                                 '(url "http://neal2500k:8080/GetStatsAjax")
                                                 '(crossDomain #t))))

                       (! map (x _) (get! x.response))
                       (! pairwise)

                       (! map (pairArray _)
                          (define old (vector-ref pairArray 0))
                          (define new (vector-ref pairArray 1))

                          (define oldVec (call! Object.entries old))
                          (define oldAssoc (for/list ((a (in-vector oldVec)))
                                             (apply cons (vector->list a))))

                          (define newVec (call! Object.entries new))
                          (define newAssoc (for/list ((a (in-vector newVec)))
                                             (apply cons (vector->list a))))
                                   
                          (for/list ((key-value (in-list newAssoc))
                                     #:do ((match-define (cons key-new value-new) key-value))
                                     #:do ((match-define (cons _       value-old) (assoc key-new oldAssoc)))
                                     #:when (> value-new value-old))
                                      
                            (list key-new value-old value-new (- value-new value-old))))))))

               (ajax-changes-obs
                (~~>
                 ajax-ticks-obs
                
                 (! filter (diff-list _)
                    (define result (not (null? diff-list)))
                    (when result (js-log-fmt "(not (null? diff-list)) = ~a" result))
                    result
                    ))))


          (call! ajax-ticks-obs.subscribe
                 (js-object*
                  `(next
                    ,(ext-λ (_)
                            (js-log "did ajax")
                            ))))

          (call! ajax-changes-obs.subscribe
                 (js-object*
                  `(next
                    ,(ext-λ (x)
                            (js-log** "in subscribe again. external-lambda x: " x)
                            (js-log "done showing value")
                            )))))
        (void))


