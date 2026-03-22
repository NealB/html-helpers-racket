;(include-lib html-helpers-webracket)
(include/reader "html-helpers-webracket.rkt" read-syntax/skip-first-line)

(define test-sxml
  (renderHtmlElement
   `(div
     (Stylesheet (href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.8/dist/css/bootstrap.min.css"))

     ;(script (src "pretty-print.js"))
(script (src "https://unpkg.com/prettier@3.8.1/standalone.js"))
(script (src "https://unpkg.com/prettier@3.8.1/plugins/html.js"))
(script "
function prettyPrintHtml(str)
{
  prettier.format(str, {
    parser: 'html',
    plugins: prettierPlugins,
  })
    .then(x => document.getElementById('output-display').innerText = x)
    .catch(e => document.getElementById('output-display').innerText = `Prettier returned an error:\n` + e);
}
")

     (BS/Container
      (h3 "Convert s-expressions to html")

      (div
       (textarea
        |#helper-text-input| .form-control $helper-text-input
        (rows 6)
        (cols 100)
        (placeholder "Type html helper expression here")
        "(div.foo.bar
  (ul
    (li \"one\")
    (li \"two\"))
  \"Lorem ipsum\")"))

      (div
       (button:button
        .btn.btn-success
        |#convert-helper-string|
        "Convert"))

      (pre
       |#output-display|
       )))))

(define content
  (sxml->dom (car test-sxml)))

(define body (js-document-body))
(js-append-child! body content)


(define (handle-convert-button-click event)
  (define helper-text-input (js-get-element-by-id "helper-text-input"))
  (define str (js-ref helper-text-input "value"))
  
  (define sxml-result (render_html_element str #f))

  (define xml-result (sxml->html (car sxml-result)))

  (define js-expression (format "prettyPrintHtml('~a')" xml-result))
  (js-eval js-expression))

(define convert-button (js-get-element-by-id "convert-helper-string"))

(define convert-button-callback (procedure->external handle-convert-button-click))
(js-add-event-listener! convert-button "click" convert-button-callback)

