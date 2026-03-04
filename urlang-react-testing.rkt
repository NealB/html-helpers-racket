#lang racket/base

(require racket/runtime-path racket/format racket/file
         urlang urlang/html urlang/extra urlang/react/urx urlang/for
         syntax/parse racket/syntax racket/pretty)
(require "HtmlHelpersRacket.rkt")

(define-urlang-macro use-state
  (λ (stx)
    (syntax-parse stx
      
      [(_ state-id set-state-id initial-expr)
       
       (syntax/loc stx
         (var [state-temp   (React.useState initial-expr)]
              [state-id     (ref state-temp 0)]
              [set-state-id (ref state-temp 1)]))])))


(define-urlang-macro render-html-elements
  (λ (stx)
    (define args (cdr (syntax->datum stx)))
    (define out
      (parameterize ((attr-prepend 'object)
                     (include-empty-atts? #t)
                     (alter-element-fn (λ (el) `(React.createElement ,(~a (car el)) ,@(cdr el)))))
        
        (let* ((result (first (renderHtmlElements args))))
          result)))
    
    (datum->syntax stx out)))


(parameterize ([current-urlang-output-file "react-example.js"])
  (urlang
   (urmodule exercise ; saved in exercise.js
     (import delete this Promise fetch Object $ React ReactDOM document document.title JSON RegExp renderHtmlElements attr-prepend include-empty-atts? alter-element-fn form-example alert quasiquote unquote unquote-splicing list div p className add1
             onClick)

     (define (App1 props)       
       (use-state count set-count 0)
       (use-state show-form set-show-form #t)

       (var [on-button-click (lambda ()
                               (set-show-form (not show-form)))])
       (var [on-button2-click (lambda ()
                               (set-count (lambda (c) (+ 1 c))))])



       (var (button-style (object (width "150px"))))
         
       (React.createElement "div" (object (className "container")) 
                            (render-html-elements (Stylesheet (href "https://cdn.jsdelivr.net/npm/bootstrap@4.6.2/dist/css/bootstrap.min.css")
                                                              (integrity "sha384-xOolHFLEh07PJGoPkLv1IbcEPTNtaed2xpHsD9ESMhqIYd0nLMwNLD69Npy4HI+N")
                                                              (crossOrigin "anonymous")))
         
                            (render-html-elements (button
                                                   (className "btn btn-success")
                                                   (onClick on-button-click)
                                                   (style button-style)
                                                   (if show-form
                                                       (RawHtml "Hide address")
                                                       (RawHtml "Show address"))))
         
                            (render-html-elements (span (Children
                                                         (button
                                                          (className "btn btn-secondary")
                                                          (onClick on-button2-click)
                                                          "Add 1")
                                                         (RawHtml (+ "Count: " count)))))
         
                            (render-html-elements 
                                 (form
                                  (.form-row
                     
                                   (.form-group .col-md-6
                                                (label (for "inputEmail4") "Email")
                     
                                                (input:email
                                                 .form-control#inputEmail4
                                                 (placeholder "Email")))
               
                                   (.form-group .col-md-6
                                                (label (for "inputPassword4") "Password")
                                                (input.form-control#inputPassword4
                                                 (type "password")
                                                 (placeholder "Password"))))
              
                                  (if show-form (.form-group
                                                 (label (for "inputAddress") "Address")
                                                 (input:text
                                                  $inputAddress.form-control#inputAddress
                                                  (placeholder "1234 Main St")))
                                      (hr))
              
                                  (.form-group
                                   (label (for "inputAddress2") "Address 2")
                                   (input:text
                                    (AbbrevAttrString ".form-control #inputAddress2")
                                    (placeholder "Apartment, studio, or floor")))
              
                                  (.form-row
                                   (.form-group .col-md-6
                                                (label (for "inputCity") "City")
                                                (:text
                                                 (AbbrevAttrString "#inputCity .form-control $inputCity")))
               
                                   (.form-group .col-md-4
                                                (label (for "inputState") "State")
                                                (select
                                                 (defaultValue "Choose...")
                                                 (AbbrevAttrString ".form-control #inputState $inputState")
                                                 (option "Choose...")
                                                 (option "...")))
               
                                   (.form-group .col-md-2
                                                (label (for "inputZip") "Zip")
                                                (BS/TextBox
                                                 (AbbrevAttrString "#inputZip $inputZip")))
              
                                   (.form-group
                                    (.form-check
                                     (input (AbbrevAttrString ".form-check-input :checkbox #gridCheck"))
                                     (label.form-check-label
                                      (for "gridCheck")
                                      "        Check me out\n"))))
              
                                  (button.btn.btn-primary
                                   (type "submit")
                                   "Sign in"))))))))
   
 
;@urx[@div{@h1{React Example}
;          @p{You have clicked @ur[count] times at the button.}
;          @button[onClick: @ur[on-button-click]]{Click me}}]))));

