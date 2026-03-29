(include/reader "webracket-util.rkt" read-syntax/skip-first-line)
(include/reader "html-helpers-webracket.rkt" read-syntax/skip-first-line)
(include/reader "reverse-engineer-html.rkt" read-syntax/skip-first-line)


(define default-helper-sexp
#<<END
(div.foo.bar.#foobar-id
  (ul
    (li "one")
    (li "two"))
  "Lorem ipsum")
END
)

(define page-desc
  `(div
    (Stylesheet (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css"))

    (script .#prettier1 (src "https://unpkg.com/prettier@3.8.1/standalone.js"))
    (script .#prettier2 (src "https://unpkg.com/prettier@3.8.1/plugins/html.js"))

    ;(script .#rxjs (src "https://unpkg.com/rxjs@%5E7/dist/bundles/rxjs.umd.min.js"))
     
    (.container-fluid
     (h3 "Convert s-expressions to html")

     (.row
      (.col
       (textarea
        .form-control .#helper-text-input $helper-text-input
        (rows 10)
        (cols 100)
        (placeholder "Type html helper expression here")
        ,default-helper-sexp))

      (.col
       (textarea
        .form-control .#output-display
        (readonly "readonly")
        (rows 10)
        )))

     (.row
      (.col
       (button:button
        .btn.btn-success
        .#convert-helper-string
        "Convert")))

     (.row
      (.col
       (textarea
        .form-control $html-input.#html-input
        (rows 10)
        (cols 100)
        (placeholder "Type html here")))

      (.col
       (textarea
        .form-control .#reverse-output-display
        (readonly "readonly")
        (rows 10)
        )))
     )))


(define page-sxml
  (renderHtmlElement page-desc))

(import-js-symbols document)

(define (get-elements-by-class-name c)
  (call! document.getElementsByClassName c))

(define content
  (sxml->dom (car page-sxml)))

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

(for ((id '("prettier1" "prettier2")))
  (define element (js-get-element-by-id id))

  (on-do! element load _
          (set! prettier-loaded-count (+ 1 prettier-loaded-count))
          (format-when-ready)))


(assign! global.split_attribute_short_strings (procedure->external split_attribute_short_strings))
(assign! global.render_html_element (procedure->external render_html_element))

(include/reader "rxjs-playing.rkt" read-syntax/skip-first-line)
