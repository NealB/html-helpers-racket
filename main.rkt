(include/reader "html-helpers-webracket.rkt" read-syntax/skip-first-line)
(require (for-syntax racket/base racket/match racket/string racket/format))

(define test-sxml
  (renderHtmlElement
   `(div
     (Stylesheet (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css"))

(script (src "https://unpkg.com/prettier@3.8.1/standalone.js"))
(script (src "https://unpkg.com/prettier@3.8.1/plugins/html.js"))

(BS/Container
 (h3 "Convert s-expressions to html")

 (div
  (textarea
   .#helper-text-input .form-control $helper-text-input
   (rows 6)
   (cols 100)
   (placeholder "Type html helper expression here")
   "(div.foo.bar.#foobar-id
  (ul
    (li \"one\")
    (li \"two\"))
  \"Lorem ipsum\")"))

 (div
  (button:button
   .btn.btn-success
   .#convert-helper-string
   "Convert"))

 (pre
  .#output-display
  )))))

(define content
  (sxml->dom (car test-sxml)))

(define body (js-document-body))
(js-append-child! body content)

(define global (js-global-this))

(define-syntax (call! stx)
  (match-define (list _ dotted-name args ...) (syntax->list stx))
  (define str (~a (syntax->datum dotted-name)))
  (match-define (list obj method) (string-split str "."))
  (datum->syntax stx
                 `(js-send ,(string->symbol obj) ,method  (vector ,@args))))

(define-syntax (assign! stx)
  (match-define (list _ dotted-name rvalue) (syntax->list stx))
  (define str (~a (syntax->datum dotted-name)))
  (match-define (list obj property) (string-split str "."))
  (datum->syntax stx
                 `(js-set! ,(string->symbol obj) ,property  ,rvalue)))

(define-syntax-rule (define-proc-external name proc-expr)
  (define name (procedure->external proc-expr)))

;(define-syntax-rule (define-by-id name proc-expr)
;  (define name (procedure->external proc-expr)))



(define output-display (js-get-element-by-id "output-display"))
  
(define (pretty-print-html str)
 

  
  (define prettierPlugins (js-ref/extern (js-global-this) "prettierPlugins"))
    
  (define prettier-options (js-object (vector #("parser" "html") (vector "plugins" prettierPlugins))))
  
  (define prettier (js-ref/extern (js-global-this) "prettier"))

  
  (define format-result-promise (call! prettier.format str prettier-options))
  
  
  (define (prettier-format-callback x)
    (assign! output-display.innerText x)
    (void))

  
  (call! format-result-promise.then (procedure->external prettier-format-callback))


  (void))
  
(define helper-text-input (js-get-element-by-id "helper-text-input"))


(define (handle-convert-button-click event)

  (with-handlers ([(lambda _ #t) (lambda (e)
                                   (match-define (exn:fail:read msg _ _) e)
                                   ((assign! output-display.innerText (format "Prettier returned an error: ~a" msg))))])

    (define str (js-ref helper-text-input "value"))
    (define sxml-result (render_html_element str #f))
    (define xml-result (sxml->html (car sxml-result)))

    (pretty-print-html xml-result)
    (void)))    

(define convert-button (js-get-element-by-id "convert-helper-string"))

;(define convert-button-callback (procedure->external handle-convert-button-click))

(define-proc-external convert-button-callback handle-convert-button-click)

(js-add-event-listener! convert-button "click" convert-button-callback)

(js-add-event-listener! helper-text-input "input" convert-button-callback)



(assign! global.split_attribute_short_strings (procedure->external split_attribute_short_strings))
(assign! global.render_html_element (procedure->external render_html_element))

