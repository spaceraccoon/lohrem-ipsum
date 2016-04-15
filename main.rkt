#lang web-server/insta

; start: request -> response
; Consumes a request, and produces a page that displays all of the
; web content.
(define (start request)
  (response/xexpr
   `(html (head (title "Lohrem Ipsum"))
          (body (h1 "Lohrem Ipsum")
                ,(render-words)))))
 
(define (render-words)
  `(div ((class "result"))
        ,@(select-words 3 singlish-dictionary)))

(define singlish-dictionary
  (list
   "lor"
   "lah"
   "leh"
   "walao"
   ))

(define (select-words n lst)
  (cond
    [(= n 0) '(".")]
    [(= n 1) (cons (list-ref lst (random (length lst))) (select-words (- n 1) lst))]
    [else (append (cons (list-ref lst (random (length lst))) '(" ")) (select-words (- n 1) lst))]))
        