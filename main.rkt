#lang web-server/insta

; start: request -> response
; Consumes a request, and produces a page that displays all of the
; web content.
(define (start request)
  (response/xexpr
   `(html (head (title "Lohrem Ipsum"))
          (body (h1 "Lohrem Ipsum")
                ,(render-text 3)))))

(define (render-text n)
  `(div ((class "lol"))
        ,@(render-paragraphs n)))

(define (render-paragraphs n)
  (if (= n 0)
      '()
      (cons (render-paragraph) (render-paragraphs (- n 1)))))

(define (render-paragraph)
  `(p ,(generate-paragraph (random 15 25))))

(define (capitalize str)
  (string-set! str 0 (char-upcase (string-ref str 0)))
  str)

(define (generate-paragraph n)
  (let ([sentence (capitalize (generate-sentence (random 5 10) singlish-dictionary))])
    (if (= n 1)
        sentence
        (string-append sentence " " (generate-paragraph (- n 1))))))
      
(define (render-words)
  `(div ((class "sentence"))
        ,@(string-titlecase (generate-sentence 3 singlish-dictionary))))

(define singlish-dictionary
  (list
   "lor"
   "lah"
   "leh"
   "walao"
   "sian"
   "aiyo"
   "bodoh"
   "kambing"
   "sayang"
   "liao"
   "liddat"
   "blur"
   ))

(define (generate-sentence n lst)
  (if (= n 1)
      (string-append (list-ref lst (random (length lst))) ".")
      (string-append (list-ref lst (random (length lst))) " " (generate-sentence (- n 1) lst))))
        
