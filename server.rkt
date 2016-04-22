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
; Words chosen at random from the dictionary.
(define (generate-sentence n lst)
  (if (= n 1)
      (string-append (list-ref lst (random (length lst))) ".")
      (string-append (list-ref lst (random (length lst))) " " (generate-sentence (- n 1) lst))))

; render-paragraphs: words -> xexpr
; Based on number of words requested, produces a list of xexpr fragments of words.
(define (render-words n)
  (list `(p ,(generate-words n))))

; Recursively selects for the right number of words in sentences.
(define (generate-words n)
  (let ([current-length (random 5 10)])
    (if (<= n 10)
        (capitalize (generate-sentence n singlish-dictionary))
        (string-append (capitalize (generate-sentence current-length singlish-dictionary)) " " (generate-words (- n current-length))))))

; Singlish dictionary as a list of a strings
(define singlish-dictionary
  (list
   "abuden"
   "abit"
   "action"
   "agak-agak"
   "ahbeng"
   "ahlian"
   "aiyo"
   "alamak"
   "angmoh"
   "arrow"
   "atas"
   "auntie"
   "belanja"
   "blur"
   "bodoh"
   "bochup"
   "bojio"
   "boliao"
   "bopian"
   "chao"
   "charbor"
   "cheebai"
   "chim"
   "chiobu"
   "chiong"
   "chope"
   "dey"
   "dulan"
   "echerly"
   "encik"
   "gabra"
   "gahmen"
   "gahrang"
   "gehkiang"
   "goondu"
   "gostan"
   "haolian"
   "heng"
   "horlan"
   "jiak"
   "jialat"
   "kaypoh"
   "kayu"
   "kena"
   "kiasu"
   "kiasi"
   "kilat"
   "kopi"
   "lah"
   "leh"
   "lepak"
   "lor"
   "loh"
   "macam"
   "makan"
   "malu"
   "meh"
   "mug"
   "nia"
   "obiang"
   "orh"
   "paikia"
   "paktor"
   "rabak"
   "sei"
   "sekali"
   "shiok"
   "sia"
   "siam"
   "siao"
   "sibeh"
   "simi"
   "sotong"
   "stun"
   "suay"
   "sui"
   "swaku"
   "tahan"
   "tapau"
   "taupok"
   "teh"
   "tekan"
   "ulu"
   "walao"
   "wapiang"
   "waseh"
   "yandao"
   "zai"
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
  (define output
    (cond
      [(can-parse-input? (request-bindings request))
           (parse-input (request-bindings request))]
      [else '(`(p ,"Don't play play, I can make 200 paragraphs or 50000 words!"))]))
  (render-page output request))
 
 
; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
; Checks if bindings are correct input and format
(define (can-parse-input? bindings)
  (and (exists-binding? 'type bindings)
       (exists-binding? 'length bindings)
       (exact-positive-integer? (string->number (extract-binding/single 'length bindings)))
       (or (and
            (equal? (extract-binding/single 'type bindings) "paragraph")
            (<= (string->number (extract-binding/single 'length bindings)) 200))
           (and
            (equal? (extract-binding/single 'type bindings) "word")
            (<= (string->number (extract-binding/single 'length bindings)) 50000)))))
 
; parse-post: bindings -> post
; Consumes a bindings, and produces a post out of the bindings.
(define (parse-input bindings)
  (if (equal? (extract-binding/single 'type bindings) "paragraph")
      (render-paragraphs (string->number (extract-binding/single 'length bindings)))
      (render-words (string->number (extract-binding/single 'length bindings)))))
 
; render-page: output request -> response
(define (render-page output request)
  (response/xexpr
   `(html (head (title "Lohrem Ipsum")
                (link ((rel "stylesheet")
                       (href "https://s3.amazonaws.com/lohrem-ipsum/stylesheet.css")
                       (type "text/css"))))
          (body (div ((class "container"))
                     (div ((class "header"))
                          (h1 "Lohrem Ipsum")
                          (p "A Singlish Lorem Ipsum genderator coded in Racket/Scheme."))
                     (div ((class "wrapper"))
                (div ((class "input"))
                     (form ((id "parameters"))
                      (input ((name "length")))
                      (select ((form "parameters") (name "type"))
                             (option ((value "paragraph")) "Paragraphs")
                             (option ((value "word")) "Words"))
                      (input ((type "submit") (value "Generate")))))
                (div ((class "output"))
                     ,@output)))
                (div ((class "footer"))
                     (p "A Racket/Scheme webapp coded by "(a ((href "http://limzhiweieugene.github.io/")) "Eugene Lim")". Image by "(a ((href "http://lemongraphic.sg/")) "Lemongraphic")". Background texture by "(a ((href "http://flashboard.pl/")) "Michal")"."))))))

; start servlet
(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port
               #:command-line? #t)
