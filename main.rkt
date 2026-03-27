(include/reader "webracket-util.rkt" read-syntax/skip-first-line)
(include/reader "html-helpers-webracket.rkt" read-syntax/skip-first-line)

(define test-sxml
  (renderHtmlElement
   `(div
     (Stylesheet (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css"))

     (script .#prettier1 (src "https://unpkg.com/prettier@3.8.1/standalone.js"))
     (script .#prettier2 (src "https://unpkg.com/prettier@3.8.1/plugins/html.js"))

     (.container-fluid
      (h3 "Convert s-expressions to html")

      (.row
       (.col
        (textarea
         .#helper-text-input .form-control $helper-text-input
         (rows 10)
         (cols 100)
         (placeholder "Type html helper expression here")
#<<END
(div.foo.bar.#foobar-id
  (ul
    (li "one")
    (li "two"))
  "Lorem ipsum")
END
))

       (.col
        (textarea
         .#output-display .form-control
         (readonly "readonly")
         (rows 10)
         )))

      (.row
       (.col
        (button:button
         .btn.btn-success
         .#convert-helper-string
         "Convert")))))))

(define content
  (sxml->dom (car test-sxml)))

(define body (js-document-body))
(js-append-child! body content)


(define output-display (html# output-display))


(define (display-converted continuation)
  
  (with-handlers ([exn? (lambda (e)
                          (match-define (exn:fail:read msg _ _) e)
                          (continuation (format "~a" msg)))])

    (define str (js-ref (html# helper-text-input) "value"))
    (define sxml-result (render_html_element str))
    (define html-result (sxml->html (car sxml-result)))

    (import-js-symbols prettier prettierPlugins)
  
    (define format-result-promise (call! prettier.format
                                         html-result
                                         (js-object (vector #("parser" "html")
                                                            `#("plugins" ,prettierPlugins)))))
  

    (call! format-result-promise.then (procedure->external continuation))    
    (void)))


(define prettier-loaded-count 0)

(define (format-when-ready)
  (when (= prettier-loaded-count 2)
    (display-converted write-output-display-value)))

(define (write-output-display-value s)
  (assign! output-display.value s))

(on-do! (html# convert-helper-string) click _
        (display-converted write-output-display-value))

(on-do! (html# helper-text-input) input _
        (display-converted write-output-display-value))

(on-do! (html# prettier1) load _
        (set! prettier-loaded-count (+ 1 prettier-loaded-count))
        (format-when-ready))

(on-do! (html# prettier2) load _
        (set! prettier-loaded-count (+ 1 prettier-loaded-count))
        (format-when-ready))


(assign! global.split_attribute_short_strings (procedure->external split_attribute_short_strings))
(assign! global.render_html_element (procedure->external render_html_element))

