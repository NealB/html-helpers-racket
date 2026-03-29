#lang webracket

(define rxjs-script
  (renderHtmlElement '(script .#rxjs (src "https://unpkg.com/rxjs@%5E7/dist/bundles/rxjs.umd.min.js"))))

(js-append-child! (js-document-head) (sxml->dom (car rxjs-script)))

#| (on-do! (html# rxjs) load _
           (js-log "rxjs loaded")
           
           (import-js-symbols rxjs)
   
           (js-log rxjs)
   
   
           (define rxjs-obs
             (call! rxjs.concat
                    (call! rxjs.from '#(0))
                    (call! rxjs.timer 0 1000)))
   
           (call! rxjs-obs.subscribe
                  (external-lambda (x)
                     (js-log (format "in subscribe. external-lambda: ~a" x))
                     ;(js-log x)
                  ))) |#

