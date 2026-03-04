#lang racket
(require racket/match racket/list racket/string racket/format threading sxml racket/pretty html-printer net/cgi racket/function racket/local loop)
(require (prefix-in rlg: racklog))
(require (prefix-in pprint: pprint))
(require racket/generator)
;(require "HtmlHelperMacros.rkt")
;(require "macros.rkt")
;(require "HtmlHelperExample.rkt")

(define attr-prepend (make-parameter 'object))
(define include-empty-atts? (make-parameter #t))
(define alter-element-fn (make-parameter #f))
(define output-type (make-parameter 'react))

(define html_elements '(a abbr address area article aside b base blockquote body br button canvas caption cite code col colgroup data datalist dd del details dfn dialog div dl dt em embed fieldset figure footer form h1 h2 h3 h4 h5 h6 head header hgroup
                          hr html i iframe img input ins keygen label legend li link main map menu menuitem meta nav noscript object ol optgroup option p pre script section select small span strong sub sup table tbody td template textarea tfoot th thead title tr ul
                          Stylesheet Option ))
(define html_element_set
  (delay (list->set html_elements)))

(define (html_element_tag? tag)
  (or
   (set-member? (force html_element_set) tag)
   (and
    (symbol? tag)
    (or
     (set-member? (force html_element_set) (first (split-attribute-short-strings tag)))
     (string-prefix? (~a tag) "#")
     (string-prefix? (~a tag) ".")
     (string-prefix? (~a tag) ":")
     (string-prefix? (~a tag) "$")
     (string-prefix? (~a tag) "BS/")
     (string-prefix? (~a tag) "Call!")))))
    

;(define (html-helper-macro-find s)
;  (define sym (string->symbol s))
;  (eval sym html-helper-macro-ns))

(define (lower-case-symbol? sym)
  (and (symbol? sym) (~> sym ~a (string-ref 0) char-lower-case?)))



(define (split-by-slash sym)
  (~> sym
      ~a
      (string-split "/")))

(define (split-by-bang sym)
  (~> sym
      ~a
      (string-split "!")))


(define (split-attribute-short-strings sym-or-str)
  (define str
    (if (symbol? sym-or-str)
        (~> sym-or-str ~a)
        (~> sym-or-str (string-replace " " "" #:all? #t))))

  (let loop ((offset 0) (acc '()))
    (define m (regexp-match #px"(^|\\.|#|\\$|:)([^.#\\$:]+)" str offset))
    (if (not m)
        (reverse acc)
        (begin
          (match-let (((list m prefix suffix) m))
            (define next-offset (+ offset (string-length m)))

            (match prefix
              ("" (loop next-offset (list (string->symbol suffix))))
              ("." (loop next-offset (cons `(class ,suffix) acc)))
              ("#" (loop next-offset (cons `(id ,suffix) acc)))
              ("$" (loop next-offset (cons `(name ,suffix) acc)))
              (":" (loop next-offset (cons `(type ,suffix) acc)))
              ))))))
       
(define (sxmlFromSimpleFacets facets0)
  
  (let* ((facets1 (rewriteStringFacets facets0))
         (outer-tag (get-tag facets1)))

    (cond
      [(eq? (car facets1) '&) (list facets1)]
      ;[(eq? (car facets1) 'if_ ) (list facets1)]
      [else
       (let*
           ((facets facets1)
            (sxmlAttributeList (generateSxmlAttributeList facets))
                         
            (tag (and~> (assoc 'Tag facets) second ensure-symbol))
                         
            (htmlContentOrFalse (assoc 'HtmlContent facets))
            (childrenOrFalse (and~> facets
                                    gatherChildren
                                    (false-if-not pair?)))
            
            ;(withAtts (if sxmlAttributeList (list tag sxmlAttributeList) (list tag)))
            
            (withAtts `(,tag  ,@(if sxmlAttributeList (list sxmlAttributeList) '())))
            
            (withContent (cond 
                           (htmlContentOrFalse (append withAtts (list (second htmlContentOrFalse))))
                           (childrenOrFalse (append withAtts (renderHtmlElements (cdr childrenOrFalse))))
                           (else withAtts))))


         (if (alter-element-fn)
             (list ((alter-element-fn) withContent))
             (list withContent)))])))



(define (rewriteElementStep node)

  (match node
    ((list 'BS/Select atts ...)              `(select (class "form-control") ,@atts))
    ((list 'BS/Hidden atts ...)              `(input (type "hidden") (class "form-control") ,@atts))
    ((list 'BS/TextBox atts ...)             `(input (type "text") (class "form-control") ,@atts))
    ((list 'BS/RadioButton atts ...)         `(input (type "radio") (class "form-control custom-control-input") ,@atts))
    ((list 'BS/FormGroupRow atts ...)        `(BS/Row (class "form-group") ,@atts))
    ((list 'BS/Row atts ...)                 `(div (class "row") ,@atts))
    ((list 'BS/Col atts ...)                 `(div (class "col") ,@atts))
    ((list 'BS/Container atts ...)           `(div (class "container") ,@atts))
    ((list 'BS/TableFixedLayout atts ...)    `(table (class "table") (style "table-layout: fixed") ,@atts))
        
    ((list 'Element atts ...)                `(#:Finished ,@atts))
    ((list 'Elements atts ...)               `(#:Finished ,@(map rewriteElement atts)))
    ((list '& atts ...)                      `(#:Finished `& ,@atts))
    ;((list '_if pred body1 body2)             `(#:Finished _if ,pred ,(rewriteElement body1) ,(rewriteElement body2)))
  
    ((list 'Stylesheet atts ...)             `(link (rel "stylesheet") ,@atts))
    ((list 'Option value text)                `(option (value ,value) ,text))
    ((list 'Input/Text atts ...)             `(input (type "text") ,@atts))

    ((list
      (app split-attribute-short-strings (list-rest tag class-id-list)) atts ...)
     #:when (not (null? class-id-list))
     `(,tag ,@class-id-list ,@atts))
  
    ((list (? html_element_tag? tag) atts ...) `(Element (Tag ,(~a tag)) ,@atts))

    (_ #f)))


(define (fillDefaultTag node)
  (define tag (car node))
  
  (match (~a tag)
      ((regexp #px"^[.#]") `(,(string->symbol (format "div~a" tag)) ,@(cdr node)))
      ((regexp #px"^[:$]") `(,(string->symbol (format "input~a" tag)) ,@(cdr node)))
      (_ node)))


(define (rewriteAbbrevAttrFacets node)
  (for/list ((node-elem node)
             (index (in-naturals 0)))
    (cond
      [(= index 0) node-elem]
      [(symbol? node-elem) (list 'AbbrevAttrString (~a node-elem))]
      [else node-elem])))


(define (rewriteStringFacets node)
  (map (λ (facet)
         (if (string? facet) `(HtmlContent ,facet) facet))
       node))

(define (rewriteElement node0)
  (define node (~> node0 fillDefaultTag rewriteAbbrevAttrFacets))
  
  (let loop ((node-iteration node))
    (define rewritten (rewriteElementStep node-iteration))

    (cond
      [(and (pair? rewritten) (eq? (car rewritten) '#:Finished)) (cdr rewritten)]
      [rewritten (loop rewritten)]
      [else #f])))
        


(define (gatherChildren facets)
  
  (loop next ((remaining-facets facets) (child-acc '() #:inherit))
        
        (if (null? remaining-facets)
            (and (not (null? child-acc)) `(Children ,@(reverse child-acc)))
        
            (local
              ((match-define (list facets-head facets-tail ...) remaining-facets)
           
               (define (--> . replacement-facets) (next `(,@replacement-facets ,@facets-tail)))
               (define (continue) (next facets-tail)))

              (match facets-head
                ((list 'if pred body1 body2)                         (next facets-tail #:child-acc (cons facets-head child-acc)))

                ((list (? html_element_tag?) _ ...)                  (next (cons `(Children ,facets-head) facets-tail)))

                ((list 'Children child)                               (next facets-tail #:child-acc (cons child child-acc)))
                
                ((list 'Elements elements ...)                       (--> `(Children ,@elements)))

                ((list 'Children first-child rest-of-children ...)   (--> `(Children ,first-child) `(Children ,@rest-of-children)))
            
                ((and (list 'Element _ ...) element)                 (next facets-tail #:child-acc (cons element child-acc)))

                (_                                                    (continue)))))))



(define (rewriteAttributes htmlAttributes)

  (define class-attr-name
    (if (eq? (output-type) 'react)
        'className
        'class))
  
  (define for-attr-name
    (if (eq? (output-type) 'react)
        'htmlFor
        'for))


  (let loop ((remaining-htmlAttributes htmlAttributes) (explicit-attrs '()))
        
    (if (null? remaining-htmlAttributes)
        (reverse explicit-attrs)

        (local
          ((match-define (list-rest attrs-head attrs-tail) remaining-htmlAttributes)
               
           (define (--> . replacement-attributes)
             (loop (append replacement-attributes attrs-tail) explicit-attrs))

           (define (next)
             (loop attrs-tail explicit-attrs)))
     
          (match attrs-head
            ((or #f '())                                      (next))
            
            ((and (list 'ExplicitAttribute _ _) explicit)     (loop attrs-tail (cons explicit explicit-attrs)))
            
            ((list-rest 'class tail)                          (--> `(ClassAttribute ,@tail)))
            
            ((list 'ClassAttribute (and (? string? (pregexp " ")) s)) (--> `(ClassAttribute ,@(string-split s))))
              
            ((list 'ClassAttribute (? string? s))             (--> `(ExplicitAttribute ,class-attr-name ,s)))
            ((list 'ClassAttribute s tail ...)               (--> `(ClassAttribute ,s) `(ClassAttribute ,@tail)))
            
            ((list 'for tail ...)                            (--> `(ExplicitAttribute ,for-attr-name ,@tail)))
              
            ;((list 'style tail ...)                          (--> `(StyleAttribute ,@tail)))

            ;((list 'StyleAttribute (? string? s))
            ; #:when (string-contains? s ";")
            ; #:do ((define style-list (string-split s #px"\\s*(;\\s*)+")))
            ;                                                  (--> `(StyleAttribute ,@style-list)))

            ;((list 'StyleAttribute (? string? s))
            ; #:do ((define normalized
            ;         (string-normalize-spaces s #px"\\s*(;\\s*)+" "; ")))
            ;                                                  (if (non-empty-string? normalized) (--> `(ExplicitAttribute style ,normalized)) (next)))
              
            ;((list 'StyleAttribute s tail ...)               (--> `(StyleAttribute ,s) `(StyleAttribute ,@tail)))
              
            ((list 'Attributes (list (? string? name) value)) (--> `(ExplicitAttribute ,(string->symbol name) ,value)))
            ((list 'Attributes (list (? symbol? name) value)) (--> `(ExplicitAttribute ,name ,value)))
            ((list 'Attributes (list name value) tail ...)   (--> `(Attributes (,name ,value)) `(Attributes ,@tail)))
              
            (`(CheckedProperty ,b)                        (if b (--> '(ExplicitAttribute checked "checked")) (next)))
            (`(RequiredProperty ,b)                       (if b (--> '(ExplicitAttribute required "required")) (next)))
            (`(SelectedProperty ,b)                       (if b (--> '(ExplicitAttribute selected "selected")) (next)))

            (`(WidthProperty ,w)                          (--> `(StyleAttribute ,(format "width: ~Apx" w))))
            (`(MinWidthProperty ,w)                       (--> `(StyleAttribute ,(format "min-width: ~Apx" w))))
            (`(MaxWidthProperty ,w)                       (--> `(StyleAttribute ,(format "max-width: ~Apx" w))))
    
            (`(HeightProperty ,w)                         (--> `(StyleAttribute ,(format "height: ~Apx" w))))
            (`(MinHeightProperty ,w)                      (--> `(StyleAttribute ,(format "min-height: ~Apx" w))))
            (`(MaxHeightProperty ,w)                      (--> `(StyleAttribute ,(format "max-height: ~Apx" w))))
            
            ((and (? rewriteElement) passThru)            (loop attrs-tail (cons passThru explicit-attrs)))

            ((list (? lower-case-symbol? sym))            (--> `(ExplicitAttribute ,sym ,(~a sym))))
       
            ((list (? lower-case-symbol? sym) s)          (--> `(ExplicitAttribute ,sym ,s))) ; if the initial symbol is lowercase, treat it as an attribute with that name

            (`(AbbrevAttrString ,str)
             #:do ((define tag-class-id-split (split-attribute-short-strings str)))
                                                          (apply --> tag-class-id-split))
            
            ((var passThru)                               (loop attrs-tail (cons passThru explicit-attrs))))))))


(define (generateSxmlAttributeList facets)
  "Group like attributes and concatenate them"

  (define simpleAttributeList (rewriteAttributes facets))
  
  (define (to-sxml-atts grps)
    (and (or (not (null? grps)) (include-empty-atts?))
         (if (attr-prepend)
             (cons (attr-prepend) grps)
             grps)))
  
  (define att-name-values
    (for/list ([att simpleAttributeList]
               #:when (and (pair? att) (eq? (car att) 'ExplicitAttribute))
               #:do ((define name (second att)))
               #:do ((define name-symbol (if (string? name) (string->symbol name) name)))
               #:do ((define value (third att))))
      (list name-symbol value)))

  (define result-atts
    (for/list ([grp (group-by first att-name-values)]
               #:do ((define attname (caar grp))))
               ;#:do ((define attvalue (apply ~a #:separator (if (eq? attname 'style) "; " " ") (remove-duplicates (map second grp))))))
      
      (list
       attname
       (if (or #| (eq? attname 'style) |# (eq? attname 'class) (eq? attname 'className))
           (apply ~a #:separator (if (eq? attname 'style) "; " " ") (remove-duplicates (map second grp)))
           (second (first grp))))))

  (to-sxml-atts result-atts))

(define (ensure-symbol s)
  (if (string? s)
      (string->symbol s)
      s))
 

(define (get-tag facets)
  (if (findf (compose not pair?) facets)
      #f
      (let* ((tagPair (assoc 'Tag facets))
             (tag (and tagPair (~> tagPair second ensure-symbol))))
        tag)))

(define (false-if-not value pred)
  (and (pred value) value))



(define (renderHtmlElement node )
  
  (match node
    ((list 'RawHtml s) (list s))
    
    ((list 'if pred body1 body2)
     `((if ,pred ,(car (renderHtmlElement body1)) ,(car (renderHtmlElement body2)))))
     
     
    (_ (~> node rewriteElement sxmlFromSimpleFacets))))

(define (renderHtmlElements nodes )
  (if (symbol? (first nodes))
      
      (renderHtmlElement nodes )
      
      (~>> nodes
           (filter (λ (el) (and el (not (null? el)) (pair? el))))
           (append-map (λ (el) (renderHtmlElement el))))))



(alter-element-fn
 (λ (el)
   `(React.createElement ,(~a (car el)) ,@(cdr el))))


(provide renderHtmlElements renderHtmlElement attr-prepend include-empty-atts? alter-element-fn output-type)


