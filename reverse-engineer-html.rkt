#lang webracket
;(require html-parsing sxml threading)
;(require "HtmlHelpersRacket.rkt")

(define html1
  ;"<!-- Modal -->
"<div class='modal' id='exampleModal' tabindex='-1' role='dialog' aria-labelledby='exampleModalLabel' aria-hidden='true'>
  <div class='modal-dialog' role='document'>
    <div class='modal-content'>
      <div class='modal-header'>
        <h5 class='modal-title' id='exampleModalLabel'>Modal title</h5>
        <button type='button' class='close' data-dismiss='modal' aria-label='Close'>
          <span aria-hidden='true'>&times;</span>
        </button>
      </div> <!-- Modal -->
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
  "<table class='table table-bordered' id='totals-table'>
    <colgroup>
        <col style='width: 25%'>
        <col style='width: 25%'>
        <col style='width: 25%'>
        <col style='width: 25%'>
    </colgroup>
    <tbody>

        <tr>
            <th class='left'>
                Total CPU (non-Rosetta)
            </th>
            <td class='credit-display td-no-grow' style='font-weight: bold'>
                <span id='total-cpu-non-rosetta-credit' data-credit='106270755' data-displayed-credit='106270755'>106,270,755</span>
            </td>

            <th class='right'>
                Astronomy
            </th>
            <td class='credit-display td-no-grow'>
                <span id='astronomy-credit' data-credit='261246368' data-displayed-credit='261246368' data-credit-step='296.4'>261,246,368</span>
            </td>



        </tr>

        <tr>
            <th class='left'>
                Total CPU
            </th>
            <td class='credit-display td-no-grow' style='font-weight: bold'>
                <span id='total-cpu-credit' data-credit='117888137' data-displayed-credit='117888137'>117,888,137</span>
            </td>

            <th class='right'>
                Total GPU
            </th>
            <td class='credit-display td-no-grow'>
                <span id='total-gpu-credit' data-credit='407049201' data-displayed-credit='407049201' data-credit-step='296.4'>407,049,201</span>
            </td>

        </tr>
        <tr>

            <th class='left'>
                All Biology / WCG
            </th>
            <td class='credit-display td-no-grow' style='font-weight: bold'>
                <span id='all-biology-wcg-credit' data-credit='263690970' data-displayed-credit='263690970'>263,690,970</span>
            </td>

            <th class='right'>
                Total
            </th>
            <td class='credit-display td-no-grow' style='font-weight: bold'>
                <span id='total-credit' data-credit='524641018' data-displayed-credit='524641018'>524,641,018</span>
            </td>
        </tr>
    </tbody>
</table>")

;(assign! html_2 html2)

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
   
(js-log "rev-eng1")

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

(js-log "rev-eng2")

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

(define (trim-all-ws str)
  (define (ws-prefix-cnt)
    (let loop ((cnt 0))
      (if (or (= cnt (string-length str)) (not (char-whitespace? (string-ref str cnt))))
          cnt
          (loop (add1 cnt)))))
  (define (ws-suffix-cnt)
    (let loop ((cnt 0))
      (if (not (char-whitespace? (string-ref str (- (string-length str) cnt 1))))
          cnt
          (loop (add1 cnt)))))
  (define pre (ws-prefix-cnt))
  (if (= pre (string-length str))
      ""
      (substring str pre (- (string-length str) (ws-suffix-cnt)))))
  
(js-log "rev-eng3")  

(define (convert-to-helpers-df node)
  (define name (string-downcase (get! node.nodeName)))
  ;(js-log** "node name=" (get! node.nodeName))
  (cond
    ((equal? name "#text")
     (let ((textContent (get! node.textContent)))
       (trim-all-ws textContent)))
    ((equal? name "#comment")
     "")
    ((string-prefix? name "#")
     (js-log** "got an unexpected node name:" name))
    (else
      (let* ((attributes (get! node.attributes))
             (attlist (for/list ((i (in-range 0 (get! attributes.length))))
                        (define att (call! attributes.item i))
                        ;(js-log att)
                        (list (string->symbol (get! att.name)) (get! att.value))))

             (childNodes (get! node.childNodes))
             (children (for/list ((i (in-range 0 (get! childNodes.length)))) (call! childNodes.item i)))
             (converted-children (map convert-to-helpers-df children))
             (empty-strings-removed (filter (λ (el) (or (not (string? el)) (not (equal? el "")))) converted-children))
             (child-list (map child-node-to-element empty-strings-removed)))
        `(,(string->symbol name) ,@(or attlist '())
                                 ,@(cond
                                     ((null? child-list) '())
                                     ((string? child-list) child-list)
                                     (else child-list)))))))

(js-log "rev-eng4")

(define (convert-ext arg)
  (let* ((helpers (convert-to-helpers-df arg))
         (helpers-as-vector (list->vector helpers))
         (helpers-ext (procedure->external (list->vector convert-to-helpers-df))))
    helpers-ext))

(js-log "rev-eng5")
    
;(assign! convert_to_helpers convert-ext)


(js-log "rev-eng6")
    
(assign! |#html-input|.value html1)


(js-log "rev-eng7")
    

(define (process-html-input-and-show)
  (define dom_document (js-document))
  (define dom_range (call! dom_document.createRange '()))

  (define html-input-element (html# html-input))

  (define $html-input-value (get! html-input-element.value))

  (define frag (call! dom_range.createContextualFragment $html-input-value))

  (define frag_childNodes (get! frag.childNodes))

  (define frag_root (call! frag_childNodes.item 0))

  (assign! html_2_frag_root frag_root)


  (define helpers-from-dom (convert-to-helpers-df frag_root))

  ;(js-log "about to pretty-print")
  
  (define os (open-output-string))
  (simple-pretty-write helpers-from-dom os)
  (define helper-output-string
    (get-output-string os))

  ;(js-log "here comes pretty output string:")
  ;(js-log helper-output-string)

  (assign! converted_to_helpers helper-output-string)

  
  (assign! |#html-input-rhs|.value helper-output-string)

  
  )


(process-html-input-and-show)

(on-do! (html# html-input) input _
        (process-html-input-and-show))
        
;(define (reverse-engineer html)
;  (define xexp (html->xexp html))
;  (convert-to-helpers (second (prune-blanks xexp))))

;(pretty-print helpers)

