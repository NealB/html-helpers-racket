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

(define html3
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

;(define xexp (html->xexp html3))


(define (prune-blanks sexps)
  (for/list ([el sexps]
             #:when (or (not (string? el)) (non-empty-string? (string-trim el)))
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
          ;(format "&~A;" (second sexps))
          sexps
          (local
            ((define attlist (and (pair? sexps) (begin
                                                  ;(printf "cadr sexps = ~s~n" (cadr sexps))
                                                  (pair? (cadr sexps))) (eq? (caadr sexps) '@) (cdadr sexps)))
             (define children (if attlist
                                  (cddr sexps)
                                  (cdr sexps)))
             (define converted-children (map convert-to-helpers children))
             (define child-list (map child-node-to-element converted-children)))
            `(,(car sexps) ,@(or attlist '())
                           ,@(cond
                               ((null? child-list) '())
                               ((string? child-list) child-list)
                               (else child-list)))))))

;      `(,(car sexps) ,@(or attlist '()) ,@(if (null? child-list) '() `((Children ,@child-list))))))))
      

;(define helpers
;  (~> xexp
;      prune-blanks
;      second
;      convert-to-helpers))

(define (reverse-engineer html)
  (define xexp (html->xexp html))
  (convert-to-helpers (second (prune-blanks xexp))))

;(pretty-print helpers)

