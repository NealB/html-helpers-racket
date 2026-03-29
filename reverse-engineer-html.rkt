#lang webracket
;(require html-parsing sxml threading)
;(require "HtmlHelpersRacket.rkt")

(define html1
  "<!-- Modal -->
<div class='modal' id='exampleModal' tabindex='-1' role='dialog' aria-labelledby='exampleModalLabel' aria-hidden='true'>
  <div class='modal-dialog' role='document'>
    <div class='modal-content'>
      <div class='modal-header'>
        <h5 class='modal-title' id='exampleModalLabel'>Modal title</h5>
        <button type='button' class='close' data-dismiss='modal' aria-label='Close'>
          <span aria-hidden='true'>&times;</span>
        </button>
      </div>
      <div class='modal-body'>
        ...
      </div>
      <div class='modal-footer'>
        <button type='button' class='btn btn-secondary' data-dismiss='modal'>Close</button>
        <button type='button' class='btn btn-primary'>Save changes</button>
      </div>
    </div>
  </div>
</div>")

(define html2
  "<div class='card'>
  &quot;
  <img class='card-img-top' src='/images/pathToYourImage.png' alt='Card image cap'>
  <div class='card-body'>
    <h4 class='card-title'>Card title</h4>
    <p class='card-text'>
      Some quick example text to build on the card title
      and make up the bulk of the card's content.
    </p>
    <a href='#!' class='btn btn-primary'>Go somewhere</a>
  </div>
</div>")

(assign! global.html_2 html2)

(define-proc-external (html3)
     "<form>
     <div class='form-row'>
       <div class='form-group col-md-6'>
         <label for='inputEmail4'>Email</label>
         <input type='email' class='form-control' id='inputEmail4' placeholder='Email'>
       </div>
       <div class='form-group col-md-6'>
         <label for='inputPassword4'>Password</label>
         <input type='password' class='form-control' id='inputPassword4' placeholder='Password'>
       </div>
     </div>
     <div class='form-group'>
       <label for='inputAddress'>Address</label>
       <input type='text' class='form-control' id='inputAddress' placeholder='1234 Main St'>
     </div>
     <div class='form-group'>
       <label for='inputAddress2'>Address 2</label>
       <input type='text' class='form-control' id='inputAddress2' placeholder='Apartment, studio, or floor'>
     </div>
     <div class='form-row'>
       <div class='form-group col-md-6'>
         <label for='inputCity'>City</label>
         <input type='text' class='form-control' id='inputCity'>
       </div>
       <div class='form-group col-md-4'>
         <label for='inputState'>State</label>
         <select id='inputState' class='form-control'>
           <option selected>Choose...</option>
           <option>...</option>
         </select>
       </div>
       <div class='form-group col-md-2'>
         <label for='inputZip'>Zip</label>
         <input type='text' class='form-control' id='inputZip'>
       </div>
     </div>
     <div class='form-group'>
       <div class='form-check'>
         <input class='form-check-input' type='checkbox' id='gridCheck'>
         <label class='form-check-label' for='gridCheck'>
           Check me out
         </label>
       </div>
     </div>
     <button type='submit' class='btn btn-primary'>Sign in</button>
   </form>")
   


(define (prune-blanks sexps)
  (for/list ([el (in-list sexps)]
             #:when (or (not (string? el)) (not (equal? "" (string-trim el))))
             #:when (or (not (pair? el))  (not (eq? (car el) '*COMMENT*))))
    (if (list? el)
        (prune-blanks el)
        el)))

(define (child-node-to-element node)
  (if (string? node)
      ;`(RawHtml ,node)
      node
      `(,(car node) ,@(cdr node))))


(define (convert-to-helpers sexps)
  (if (string? sexps)
      sexps
      (if (eq? (car sexps) '&)
          sexps
          (let* ((attlist (and (pair? sexps) (pair? (cadr sexps)) (eq? (caadr sexps) '@) (cdadr sexps)))
                 (children (if attlist
                               (cddr sexps)
                               (cdr sexps)))
                 (converted-children (map convert-to-helpers children))
                 (child-list (map child-node-to-element converted-children)))
            `(,(car sexps) ,@(or attlist '())
                           ,@(cond
                               ((null? child-list) '())
                               ((string? child-list) child-list)
                               (else child-list)))))))


(define (prune-blanks-df sexps)
  (for/list ([el (in-list sexps)]
             #:when (or (not (string? el)) (not (equal? "" (string-trim el))))
             #:when (or (not (pair? el))  (not (eq? (car el) '*COMMENT*))))
    (if (list? el)
        (prune-blanks el)
        el)))

(define (child-node-to-element-df node)
  (if (string? node)
      ;`(RawHtml ,node)
      node
      `(,(car node) ,@(cdr node))))


(define (convert-to-helpers-df node)
  (define name (string-downcase (get! node.nodeName)))
  (if (equal? name "#text")
      (get! node.textContent)
      ;(if (eq? (car sexps) '&)
      ;    sexps
          (let* ((attributes (get! node.attributes))
                 (attlist (for/list ((i (in-range 0 (get! attributes.length))))
                            (define att (call! attributes.item i))
                            (js-log att)
                            (list (string->symbol (get! att.name)) (get! att.value))))

                 (childNodes (get! node.childNodes))
                 (children (for/list ((i (in-range 0 (get! childNodes.length)))) (call! childNodes.item i)))
                 (converted-children (map convert-to-helpers-df children))
                 (child-list (map child-node-to-element converted-children)))
            `(,(string->symbol name) ,@(or attlist '())
                           ,@(cond
                               ((null? child-list) '())
                               ((string? child-list) child-list)
                               (else child-list))))))


(define dom_document (js-document))
(define dom_range (call! dom_document.createRange '()))

(define frag (call! dom_range.createContextualFragment html2))

;(define frag_childNodes (js-ref frag "childNodes"))

(define frag_childNodes (get! frag.childNodes))

(define frag_root (call! frag_childNodes.item 0))

(assign! global.html_2_frag_root frag_root)


(define helpers-from-dom (convert-to-helpers-df frag_root))
(define os (open-output-string))
(write helpers-from-dom os)
(define helper-output-string (get-output-string os))

(assign! global.converted_to_helpers helper-output-string)

(define reverse-output-display (html# reverse-output-display))
(assign! reverse-output-display.value helper-output-string)

;      `(,(car sexps) ,@(or attlist '()) ,@(if (null? child-list) '() `((Children ,@child-list))))))))

;(on-do! dom_document load _
;          (define reverse-output-display (html# reverse-output-display))
 ;         (assign! reverse-output-display.innerText str)
  ;        )

;(on-do! (html# convert-helper-string) click _
;        (define reverse-output-display (html# reverse-output-display))
;        (assign! reverse-output-display.value helper-output-string))
        
;(define (reverse-engineer html)
;  (define xexp (html->xexp html))
;  (convert-to-helpers (second (prune-blanks xexp))))

;(pretty-print helpers)

