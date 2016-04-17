#lang racket

; configuring for web server
(require web-server/servlet
         web-server/servlet-env)

; render-paragraphs: paragraphs -> xexpr
; Based on number of paragraphs requested, produces a list of xexpr fragments of paragraphs.
(define (render-paragraphs n)
  (if (= n 0)
      '()
      (cons (render-paragraph) (render-paragraphs (- n 1)))))

; render-paragraphs: paragraph -> xexpr
; Produces an xexpr fragment of the paragraph with randomly selected number of sentences
; within the range.
(define (render-paragraph)
  `(p ,(generate-paragraph (random 15 25))))

; Capitalizes the first letter in a setnence.
(define (capitalize str)
  (string-set! str 0 (char-upcase (string-ref str 0)))
  str)

; Generates a paragraph string consisting of the given number of sentences.
; Each sentence consists of a random number of words within the range.
(define (generate-paragraph n)
  (let ([sentence (capitalize (generate-sentence (random 5 10) singlish-dictionary))])
    (if (= n 1)
        sentence
        (string-append sentence " " (generate-paragraph (- n 1))))))

; Generates a sentence based on the given number of words.
; Words chosen at random from the dictionary
(define (generate-sentence n lst)
  (if (= n 1)
      (string-append (list-ref lst (random (length lst))) ".")
      (string-append (list-ref lst (random (length lst))) " " (generate-sentence (- n 1) lst))))

; Singlish dictionary as a list of a strings
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

; Configuring for herokuapp
; See http://lexi-lambda.github.io/blog/2015/08/22/deploying-racket-applications-on-heroku/
(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 8080))

; start: request -> response
; Consumes a request, and produces a page that displays all of the
; web content.
(define (start request)
  (response/xexpr
   `(html (head (title "Lohrem Ipsum"))
          (body (h1 "Lohrem Ipsum")
                (div ((class "output"))
                     ,@(render-paragraphs 3))))))

; start servlet
(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port
               #:command-line? #t)
