#lang webracket

;(include-lib sxml)


(define (attr-prepend) '@)
(define (include-empty-atts?) #f)
(define (alter-element-fn) #f)
(define output-type 'sxml)

(define html_elements
  '(a abbr address area article aside b base blockquote body br button
      canvas caption cite code col colgroup data datalist dd del details dfn
      dialog div dl dt em embed fieldset figure footer form h1 h2 h3 h4 h5
      h6 head header hgroup hr html i iframe img input ins keygen label
      legend li link main map menu menuitem meta nav noscript object ol
      optgroup option p pre script section select small span strong sub sup
      table tbody td template textarea tfoot th thead title tr ul Stylesheet
      Option))

#|(define-match-expander test
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr) #'(and (not #f) expr)])))|#


(define (~a x) (format "~a" x))

(define (html_element_tag? tag)
  (or
   (member tag html_elements)
   (and
    (symbol? tag)
    (or
     (member (first (split-attribute-short-strings tag)) html_elements)
     (string-prefix? (~a tag) "#")
     (string-prefix? (~a tag) ".")
     (string-prefix? (~a tag) ":")
     (string-prefix? (~a tag) "$")
     (string-prefix? (~a tag) "BS/")
     (string-prefix? (~a tag) "Call!")))))

(define (lower-case-symbol? sym)
  (and (symbol? sym)
       (let* ((str (~a sym))
              (firstchar (string-ref str 0)))
         (char-lower-case? firstchar))))
              



(define (split-by-slash sym)
  (string-split (~a sym) "/"))

(define (split-by-bang sym)
  (string-split (~a sym) "!"))

(define (get-special-prefix str)
  (define char-list (string->list str))
  (define first-char (car char-list))
  (define (char-special? c) (member c '(#\. #\# #\$ #\:)))
  (define first-char-special? (char-special? first-char))

  (if (string-prefix? str ".#")
      (get-special-prefix (substring str 1))
  
      (let loop ((list-remaining (cdr char-list)) (acc `(,first-char)))
        (if (or (null? list-remaining) (char-special? (car list-remaining)))
            (values (list->string (reverse acc)) (substring str (length acc)))
            (loop (cdr list-remaining) (cons (car list-remaining) acc))))))


(define (split-attribute-short-strings sym-or-str)
  (define str
    (if (symbol? sym-or-str)
        (~a sym-or-str)
        (list->string
         (for/list ((c (in-list (string->list sym-or-str)))
                    #:when (not (eq? c #\space)))
           c))))

  (define attr-short-strs
    (let loop ((remaining-string str) (acc '()))
      (if (string=? "" remaining-string)

          (reverse acc)

          (let-values
              (((str-prefix str-suffix) (get-special-prefix remaining-string)))
            (loop str-suffix (cons str-prefix acc))))))

  (define (has-prefix prefix)
    (λ (s)
      (and (string-prefix? s prefix) (substring s (string-length prefix)))))
  
  (map (λ (s)
         (match s
           ((app (has-prefix ".#") (? string? suffix)) `(id    ,suffix))
           ((app (has-prefix ".")  (? string? suffix)) `(class ,suffix))
           ((app (has-prefix "#")  (? string? suffix)) `(id    ,suffix))
           ((app (has-prefix "$")  (? string? suffix)) `(name  ,suffix))
           ((app (has-prefix ":")  (? string? suffix)) `(type  ,suffix))
           (_ (string->symbol s))))
       attr-short-strs))


(define (split_attribute_short_strings str)
  (let ((os (open-output-string))
        (l (split-attribute-short-strings str)))
    (print l os)
    (get-output-string os)))

(define (remove-duplicates str-list)
  (define h (make-hash))
  (for/list ((str (in-list str-list))
             #:when (not (hash-has-key? h str)))
    (hash-set! h str #t)
    str))

;;
;; generateSxmlAttributeList
;;
(define (generateSxmlAttributeList facets)

  (define simpleAttributeList (rewriteAttributes facets))
  
  (define (to-sxml-atts grps)
    (and (or (not (null? grps)) (include-empty-atts?))
         (if (attr-prepend)
             (cons (attr-prepend) grps)
             grps)))
  
  (define att-name-values
    (for/list ([att (in-list simpleAttributeList)]
               #:when (and (pair? att) (eq? (car att) 'ExplicitAttribute))
               #:do ((define name (second att)))
               #:do ((define name-symbol (if (string? name) (string->symbol name) name)))
               #:do ((define value (third att))))
      (list name-symbol value)))

  (define result-atts
    (for/list ([grp (in-list (group-by first att-name-values))]
               #:do ((define attname (caar grp))))
      
      (list
       attname
       (if (or (eq? attname 'class) (eq? attname 'className))
           (string-join (remove-duplicates (map second grp)) (if (eq? attname 'style) "; " " "))
           (second (first grp))))))

  (to-sxml-atts result-atts))



;;
;; sxmlFromSimpleFacets
;;
(define (sxmlFromSimpleFacets facets0)
  
  (let* ((facets1 (rewriteStringFacets facets0))
         (outer-tag (get-tag facets1)))

    (cond
      [(eq? (car facets1) '&) (list facets1)]
      [else
       (let*
           ((facets facets1)
            (sxmlAttributeList (generateSxmlAttributeList facets))

            (tag-facet (assoc 'Tag facets))
            (tag
             (and tag-facet
                  (ensure-symbol (second tag-facet))))

              
            
            (childrenOrFalse (false-if-not (gatherChildren facets '()) pair?))
             
            (withAtts `(,tag  ,@(if sxmlAttributeList (list sxmlAttributeList) '())))
            
            (withContent (cond 
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
  
    ((list 'Stylesheet atts ...)             `(link (rel "stylesheet") ,@atts))
    ((list 'Option value text)                `(option (value ,value) ,text))

    ((list
      (app split-attribute-short-strings (list-rest tag class-id-list)) atts ...)
     #:when (not (null? class-id-list))
     `(,tag ,@class-id-list ,@atts))
  
    ((list (? html_element_tag? tag) atts ...) `(Element (Tag ,(~a tag)) ,@atts))

    (_ #f)))

(define (rewriteElement node0)
  (define node (rewriteAbbrevAttrFacets (fillDefaultTag node0)))
    
  (let loop ((node-iteration node))
    (define rewritten (rewriteElementStep node-iteration))

    (cond
      [(and (pair? rewritten) (eq? (car rewritten) '#:Finished)) (cdr rewritten)]
      [rewritten (loop rewritten)]
      [else (printf "node0:~n") (print node0) (printf "node-iteration:~n") (print node-iteration) (printf "rewritten:~n") (print rewritten) (raise "what happened?~n")])))


(define (fillDefaultTag node)
  (define tag (car node))
  
  (match (string-ref (~a tag) 0)
    ((or #\. #\#)
       
     `(,(string->symbol (string-append "div" (~a tag))) ,@(cdr node)))
      
    ((or #\: #\$)
     `(,(string->symbol (string-append "input" (~a tag))) ,@(cdr node)))
      
    (_ node)))


(define (rewriteAbbrevAttrFacets node)
  (for/list ((node-elem (in-list node))
             (index (in-naturals 0)))
    (match node-elem
      [_ #:when (= index 0) node-elem]
      ['checked `(CheckedProperty #t)]
      ['required `(RequiredProperty #t)]
      ['selected `(SelectedProperty #t)]
      [(? symbol?)
       (list 'AbbrevAttrString (~a node-elem))]
      [_ node-elem])))


(define (rewriteStringFacets node)
  (map (λ (facet)
         (if (string? facet) `(HtmlContent ,facet) facet))
       node))




(define (gatherChildren facets child-acc) ;(child-acc '()))
  
        
  (if (null? facets)
      (and (not (null? child-acc)) `(Children ,@(reverse child-acc)))
        
      (local
        ((match-define (list facets-head facets-tail ...) facets)
           
         (define (--> . replacement-facets) (gatherChildren `(,@replacement-facets ,@facets-tail) child-acc))
         (define (continue) (gatherChildren facets-tail child-acc)))

        (match facets-head
          ((list 'if pred body1 body2)                         (gatherChildren facets-tail (cons facets-head child-acc)))
          ((list (? html_element_tag?) _ ...)                  (gatherChildren (cons `(Children ,facets-head) facets-tail) child-acc))
          ((list 'Children child)                              (gatherChildren facets-tail (cons child child-acc)))
          ((list 'Elements elements ...)                       (--> `(Children ,@elements)))
          ((list 'Children first-child rest-of-children ...)   (--> `(Children ,first-child) `(Children ,@rest-of-children)))
          ((and (list 'Element _ ...) element)                 (gatherChildren facets-tail (cons element child-acc)))

          ((? string?)                                         (--> `(HtmlContent  ,facets-head)))
          ((list 'HtmlContent html)                            (gatherChildren facets-tail (cons facets-head child-acc)))
          ((list 'Stylesheet _ ...)                            (gatherChildren facets-tail (cons facets-head child-acc)))
          (_                                                   (continue))))))


(define class-attr-name
  (if (eq? output-type 'react)
      'className
      'class))
  
(define for-attr-name
  (if (eq? output-type 'react)
      'htmlFor
      'for))


(define (rewriteAttributes htmlAttributes)


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
            

            ((list 'ClassAttribute (and (? string?) s))
             #:when (string-contains? s " ")
             (--> `(ClassAttribute ,@(string-split s))))
              
            ((list 'ClassAttribute (? string? s))            (--> `(ExplicitAttribute ,class-attr-name ,s)))
            ((list 'ClassAttribute s tail ...)               (--> `(ClassAttribute ,s) `(ClassAttribute ,@tail)))
            
            ((list 'for tail ...)                            (--> `(ExplicitAttribute ,for-attr-name ,@tail)))

            ((list 'Attributes (list (? string? name) value)) (--> `(ExplicitAttribute ,(string->symbol name) ,value)))
            ((list 'Attributes (list (? symbol? name) value)) (--> `(ExplicitAttribute ,name ,value)))
            ((list 'Attributes (list name value) tail ...)   (--> `(Attributes (,name ,value)) `(Attributes ,@tail)))
              
            (`(CheckedProperty ,b)                        (if b (--> '(ExplicitAttribute checked "checked")) (next)))
            (`(RequiredProperty ,b)                       (if b (--> '(ExplicitAttribute required "required")) (next)))
            (`(SelectedProperty ,b)                       (if b (--> '(ExplicitAttribute selected "selected")) (next)))

            (`(WidthProperty ,w)                          (--> `(StyleAttribute ,(string-append "width: " (number->string w) "px"))))
            (`(MinWidthProperty ,w)                       (--> `(StyleAttribute ,(string-append "min-width: " (number->string w) "px"))))
            (`(MaxWidthProperty ,w)                       (--> `(StyleAttribute ,(string-append "max-width: " (number->string w) "px"))))
    
            (`(HeightProperty ,w)                         (--> `(StyleAttribute ,(string-append "height: " (number->string w) "px"))))
            (`(MinHeightProperty ,w)                      (--> `(StyleAttribute ,(string-append "min-height: " (number->string w) "px"))))
            (`(MaxHeightProperty ,w)                      (--> `(StyleAttribute ,(string-append "max-height: " (number->string w) "px"))))

            ((list (? html_element_tag?) _ ...)           (next))
            
            ((list (? lower-case-symbol? sym))            (--> `(ExplicitAttribute ,sym ,(~a sym))))
       
            ((list (? lower-case-symbol? sym) s)          (--> `(ExplicitAttribute ,sym ,s))) ; if the initial symbol is lowercase, treat it as an attribute with that name

            (`(AbbrevAttrString ,str)
             #:do ((define tag-class-id-split (split-attribute-short-strings str)))
             (apply --> tag-class-id-split))
            
            (_                                            (next)))))))




(define (ensure-symbol s)
  (if (string? s)
      (string->symbol s)
      s))
 

(define (get-tag facets)
  (if (findf (λ (f) (not (pair? f))) facets)
      #f
      (let* ((tagPair (assoc 'Tag facets))
             (tag (and tagPair (ensure-symbol (second tagPair)))))
        tag)))

(define (false-if-not value pred)
  (and (pred value) value))



(define (renderHtmlElement node )

  (match node
    ((? string? s) (list s))
    ((list 'HtmlContent s) (list s))
    
    ((list 'if pred body1 body2)
     `((if ,pred ,(car (renderHtmlElement body1)) ,(car (renderHtmlElement body2)))))
     
     
    (_ (sxmlFromSimpleFacets (rewriteElement node)))))


(define (renderHtmlElements nodes)
  (append-map (λ (el) (renderHtmlElement el)) nodes))

; important
(define (js-log-format . args)
  (js-log (apply format args)))



(define (render_html_element s)
  (define v (read (open-input-string s)))
  (renderHtmlElement v))

  