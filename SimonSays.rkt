#lang racket

(require "magic-write.rkt")

(require 2htdp/universe)  ; big-bang
(require racket/gui/base) ; drawing-context% and inherits racket/draw functions
(require (only-in pict scale bitmap dc pict->bitmap))
(require rsound)
(require rsound/piano-tones)

(require srfi/1)  ; list-index function
(require srfi/14) ; char operations/functions
(require (only-in rnrs/base-6 log)) ; log function (with 2 parameters)

(require rackunit) ; testings


; music that plays after getting a sequence right
(define zelda (rs-read "ZeldaCut.wav"))
(define mario (rs-scale 0.3 (rs-read "MarioCut.wav")))
(define pokemon (rs-scale 0.2 (rs-read "PokemonCut.wav")))


;; the list of all color names
(define color-names (send the-color-database get-names))

;; given a list of names and a color, run down the list of names 
;; to find one that matches, or return false if no such color exists
(define (check-for-color names color)
  (cond [(empty? names) false]
        [else 
         (cond [(equal-rgb? color (send the-color-database find-color (first names)))
                (first names)]
               [else (check-for-color (rest names) color)])]))

;; helper function that takes in two colors and returns if
;; the rgb values of each are equal
(define (equal-rgb? c n)
  (and (= (send c red) (send n red))
       (= (send c green) (send n green))
       (= (send c blue) (send n blue))))

;; checks to see if c is a color in the-color-database
;; returns the name if it is, and false if not
(define (color c)
  (check-for-color color-names c))



;;         world definitions


;; cc = computer colors  - list of integers  - 1 2 3 4
;; uc = user colors      - list of integers  - 1 2 3 4
;; timer                 - struct
;; settings              - struct
(define-struct world (cc uc timer settings))

(define ps (make-pstream))


;; playing?        - boolean
;; counterGreen    - integer
;; counterRed      - integer
;; counterYellow   - integer
;; counterBlue     - integer
;; counterComputer - integer
;; cur-list        - integer
;; wait            - integer

;; playing? = true or false, if playing the game
;; counters are for highlighting colors when keys are pressed
;; cur-list is the position in the computer list 
;  that the drawing function is playing
;; wait is a timer for waiting after the sequence has been entered
;; right to play the clip of sound (zelda mario or pokemon, or none)
(define-struct timer (playing? counterGreen counterRed counterYellow
                               counterBlue counterComputer cur-list wait))


;; name         - string  - default = player
;; difficulty   - string  - easy medium hard
;; sounds       - string  - normal zelda mario pokemon
;; key-bindings - struct  - default = 1 2 3 4
;; selected-box - string  - name tl tr bl br
;; cur-page     - string  - main info highscores settings more
;; colors       - struct  - default = "green" "red" "yellow" "blue"

;; settings contains the settings stored on thce settings page
;; selected-box is the box that the user is currently editing
;; default for selected-box is the name field
;; cur-page is what page you are on
(define-struct settings (name difficulty sounds key-bindings
                              selected-box cur-page colors))


;; tl - string  - default = 1
;; tr - string  - default = 2
;; bl - string  - default = 3
;; br - string  - default = 4

;  tl = top left     --  tr = top right
;  bl = bottom left  --  br = bottom right
(define-struct key-bindings (tl tr bl br))


;;; colors from racket/draw
;; tl - color  - default = green
;; tr - color  - default = red
;; bl - color  - default = yellow
;; br - color  - default = blue
;; bg - color  - default = white
;; cc - color  - default = black
;; uc - color  - default = pink

; bg = backgroud color
; cc = color shown when computer plays a note in the sequence
; uc = color shown when user plays a note in the sequence

(define-struct main-colors (tl tr bl br bg cc uc))

(define-struct colors (main-colors selected-color selected-slider sliders color-name))


; name  - string
; score - integer

; for storing score in text file
(define-struct score (name score) #:transparent)


(define default-timer (make-timer #f 0 0 0 0 0 0 0))
(define default-keys (make-key-bindings "1" "2" "3" "4"))
(define default-main-colors (make-main-colors (send the-color-database find-color "green")
                                              (send the-color-database find-color "red")
                                              (send the-color-database find-color "yellow")
                                              (send the-color-database find-color "blue")
                                              (send the-color-database find-color "white")
                                              (send the-color-database find-color "black")
                                              (send the-color-database find-color "pink")))
(define default-colors (make-colors default-main-colors "tl" #f (list 0 255 0) "green"))
(define default-settings (make-settings "player" "easy" "normal" default-keys
                                        "name" "main" default-colors))
(define default-world (make-world (list) (list) default-timer default-settings))

;;  world -> null
; testing/debugging functions
(define (print-world w)
  (cond [(not (world? w)) ""]
        [else (displayln (string-append (print-cc (world-cc w)) 
                                        "\n" 
                                        (print-uc (world-uc w))))]))
(define (print-cc w)
  (cond [(empty? w) ""]
        [else (string-append (number->string (first w))
                             " "
                             (print-cc (rest w)))])) 
(define (print-uc w)
  (cond [(empty? w) ""]
        [else (string-append (number->string (first w))
                             " "
                             (print-uc (rest w)))])) 



; notes used for default simon button sounds
; E  - 76
; A  - 81
; C# - 85
; E  - 88



;;  world string -> world
(define (onkey w e)
  ; e is the key pressed
  (define s (world-settings w))
  (define mc (colors-main-colors (settings-colors s)))
  (define sc (colors-selected-color (settings-colors s)))
  (define cn (colors-color-name (settings-colors s)))
  (define n (settings-name s))
  (define sb (settings-selected-box s))
  (define kb (settings-key-bindings s))
  (define tl (key-bindings-tl kb)) ; top left
  (define tr (key-bindings-tr kb)) ; top right
  (define bl (key-bindings-bl kb)) ; bottom left 
  (define br (key-bindings-br kb)) ; bottom right
  (cond 
    [(and (timer-playing? (world-timer w))
          (or (key=? e tl)
              (key=? e tr)
              (key=? e bl)
              (key=? e br)))
     
     (define cc (world-cc w))
     ; append e to the end of uc (user pressed keys)
     (define uc (flatten (cons (world-uc w)
                               (cond [(key=? e tl) 1]
                                     [(key=? e tr) 2]
                                     [(key=? e bl) 3]
                                     [(key=? e br) 4]))))
     ; current timers for each color
     (define t1 (timer-counterGreen (world-timer w)))
     (define t2 (timer-counterRed (world-timer w)))
     (define t3 (timer-counterYellow (world-timer w)))
     (define t4 (timer-counterBlue (world-timer w)))
     ; m is the max of each counter, and is based on the difficulty setting
     ; shorter counters for harder difficulty
     ; - when this number is smaller, each color lights up for less time
     (define m (cond [(string=? (settings-difficulty s) "easy") 20]
                     [(string=? (settings-difficulty s) "medium") 15]
                     [(string=? (settings-difficulty s) "hard") 10]))
     
     
     (cond 
       ; if the user has entered the entire sequence right
       [(and (= (length cc) (length uc)) (right-so-far? cc uc))
        (define init-wait-time 10)
        ; set wait time to play the clip of the sound and
        ; wait until it is done playing to start the next pattern
        (define wait-time (cond [(string=? (settings-sounds s) "normal") 0]
                                [else (+ (* 
                                          (length (world-cc w))
                                          m) 
                                         init-wait-time)]))
        ;; new world with random number in computer colors
        ;; list and an empty user colors list
        (make-world (flatten (cons (world-cc w) 
                                   (add1 (random 4))))
                    (list)
                    (cond [(string=? e tl) (make-timer #t m  t2 t3 t4 m 1 wait-time)]
                          [(string=? e tr) (make-timer #t t1 m  t3 t4 m 1 wait-time)]
                          [(string=? e bl) (make-timer #t t1 t2 m  t4 m 1 wait-time)]
                          [(string=? e br) (make-timer #t t1 t2 t3 m  m 1 wait-time)])
                    s)]
       ; if the user has entered the sequence right so far
       [(right-so-far? cc uc)
        ; if the key hit is the same as the key binding for one of the buttons
        (cond [(string=? e tl) (make-world cc uc (make-timer #t m  t2 t3 t4 0 0 0) s)]
              [(string=? e tr) (make-world cc uc (make-timer #t t1 m  t3 t4 0 0 0) s)]
              [(string=? e bl) (make-world cc uc (make-timer #t t1 t2 m  t4 0 0 0) s)]
              [(string=? e br) (make-world cc uc (make-timer #t t1 t2 t3 m  0 0 0) s)])]
       ; if the user has entered a wrong key
       ; - end the game, save final score, return to main menu
       [else
        ; this block is for writing the score to the scores text file
        (define cur-score (make-score n (sub1 (length cc))))
        (define FILE "scores.txt")
        (cond [(not (file-exists? FILE))
               ;; open out
               (define out (open-output-file FILE))
               (display (write-to-string (list cur-score)) out)
               (close-output-port out)
               ;; close out
               ]
              [else    
               (define my-table (list (list "score" make-score)))
               ;; open in
               (define in (open-input-file FILE))
               
               ;using strings
               (define scores-from-file 
                 (string->struct/maker my-table
                                       (port->string in)))
               
               ;using vectors
               (define scores-from-file2 
                 (vectors->structs my-table
                                   (read in)))
               
               ; not used but could be useful
               (define insertion-pos (list-index (lambda (x) (< (score-score x) 
                                                                (score-score cur-score))) 
                                                 scores-from-file))
               ; list in front of where the score is to be inserted
               (define front (filter (lambda (x) (>= (score-score x)
                                                     (score-score cur-score)))
                                     scores-from-file))
               ; list in back of where the score is to be inserted
               (define back (filter (lambda (x) (< (score-score x)
                                                   (score-score cur-score)))
                                    scores-from-file))
               (close-input-port in)
               ;; close in
               
               ;; insert in correct position
               (define new-scores (flatten (list front
                                                 cur-score
                                                 back)))
               ;; open out
               (define out (open-output-file FILE 
                                             #:exists 'truncate
                                             ))
               ; write new scores to file
               (display (write-to-string new-scores) out)
               (close-output-port out)
               ;; close out
               
               ])
        ; return to main menu
        ; return world with playing? = false and cc and uc are reset
        (make-world (list) (list) default-timer s)
        ])]
    ; if another key is pressed or key is pressed while in a menu
    [(and (string=? (settings-cur-page s) "settings")
          (string=? sb "name")
          (< (string-length e) 2)
          ;;; name can only contain numbers and letters
          (char-set-contains? char-set:letter+digit
                              (first (char-set->list (string->char-set e))))
          ; name can be a max of 10 characters long
          (< (string-length n) 10))
     (change-val w "name" (string-append n e))]
    ; if the backspace key is hit, delete a character from the end
    [(and (string=? (settings-cur-page s) "settings")
          (string=? sb "name")
          (string=? e "\b")
          (positive? (string-length n)))
     (change-val w "name" (substring n 0 (sub1 (string-length n))))]
    ; if the selected box is one of the key binding boxes
    [(and (string=? (settings-cur-page s) "settings")
          (or (string=? sb "tl")
              (string=? sb "tr")
              (string=? sb "bl")
              (string=? sb "br"))
          (char-set-contains? 
           char-set:graphic
           (first (char-set->list (string->char-set e)))))
     (change-val w "kb" 
                 (cond [(string=? sb "tl") (make-key-bindings e  tr bl br)]
                       [(string=? sb "tr") (make-key-bindings tl e  bl br)]
                       [(string=? sb "bl") (make-key-bindings tl tr e  br)]
                       [(string=? sb "br") (make-key-bindings tl tr bl e )]))]
    
    
    [(and (string=? (settings-cur-page s) "more")
          (< (string-length e) 2)
          ;;; color name can only contain numbers and letters and spaces
          (or (key=? e " ")
              (char-set-contains? char-set:letter+digit
                                  (first (char-set->list (string->char-set e)))))
          ; name can be a max of 20 characters long
          (< (string-length cn) 20)) 
     (define (color-name? n)
       (send the-color-database find-color n))
     (define new-w (change-val w "cn" (string-append cn e)))
     (define new-cn (colors-color-name (settings-colors (world-settings new-w))))
     (change-val new-w "mc" (make-main-colors 
                             (if (and (string=? sc "tl") (color-name? new-cn))
                                 (color-name? new-cn) (main-colors-tl mc))
                             (if (and (string=? sc "tr") (color-name? new-cn))
                                 (color-name? new-cn) (main-colors-tr mc))
                             (if (and (string=? sc "bl") (color-name? new-cn))
                                 (color-name? new-cn) (main-colors-bl mc))
                             (if (and (string=? sc "br") (color-name? new-cn))
                                 (color-name? new-cn) (main-colors-br mc))
                             (if (and (string=? sc "bg") (color-name? new-cn))
                                 (color-name? new-cn) (main-colors-bg mc))
                             (if (and (string=? sc "cc") (color-name? new-cn))
                                 (color-name? new-cn) (main-colors-cc mc))
                             (if (and (string=? sc "uc") (color-name? new-cn))
                                 (color-name? new-cn) (main-colors-uc mc))))
     ]
    
    ; if the backspace key is hit, delete a character from the end
    [(and (string=? (settings-cur-page s) "more")
          (string=? e "\b")
          (positive? (string-length cn)))
     (change-val w "cn" (substring cn 0 (sub1 (string-length cn))))]
    
    ; if the key hit does not meet requirements set in above conds
    [else w])
  )



;; helper function to see if the user has entered
;; the keys right so far
;; - uses the list=? helper function
(define (right-so-far? cc uc)
  (cond [(list=? (take cc (length uc)) uc) #t]
        [else #f]))

; - assumes the lists are the same length
;; makes sure each item in uc = each item in cc
(define (list=? lst1 lst2)
  (cond [(empty? lst1) #t]
        [(= (first lst1) (first lst2))
         (list=? (rest lst1) (rest lst2))]
        [else #f]))


;;  world number number string -> world
(define (onmouse w x y e)
  (define s (world-settings w))
  (define mc (colors-main-colors (settings-colors s)))
  (define tl (main-colors-tl mc))
  (define tr (main-colors-tr mc))
  (define bl (main-colors-bl mc))
  (define br (main-colors-br mc))
  (define bg (main-colors-bg mc))
  (define cc (main-colors-cc mc))
  (define uc (main-colors-uc mc))
  (define sc (colors-selected-color (settings-colors s)))
  (define ss (colors-selected-slider (settings-colors s)))
  
  (cond 
    [(string=? e "button-down")
     (define tp? (timer-playing? (world-timer w)))
     (define main?     (string=? (settings-cur-page s) "main"      ))
     (define settings? (string=? (settings-cur-page s) "settings"  ))
     (define hs?       (string=? (settings-cur-page s) "highscores"))
     (define info?     (string=? (settings-cur-page s) "info"      ))
     (define more?     (string=? (settings-cur-page s) "more"      ))
     (define kb (settings-key-bindings s))
     (define sb (settings-selected-box s))
     
     (cond 
       [tp?
        (cond 
          [(and (< x 250) (< y 250))  ; tl
           (onkey w (key-bindings-tl kb))
           ]
          [(and (> x 250) (< y 250))  ; tr
           (onkey w (key-bindings-tr kb))
           ]
          [(and (< x 250) (> y 250))  ; bl
           (onkey w (key-bindings-bl kb))
           ]
          [(and (> x 250) (> y 250))  ; br
           (onkey w (key-bindings-br kb))
           ]
          [else w] ; x or y = 250
          )]
       ; main menu
       [(and main? (not tp?))
        (cond 
          ; start button
          [(and (> x 100) (< x 400)
                (> y 200) (< y 300))
           (start w)]
          ; settings button
          [(and (> x 450)
                (> y 450))
           (change-val w "page" "settings")]
          ; high scores button
          [(and (> x 450)
                (< y 50))
           (change-val w "page" "highscores")]
          ; information button
          [(and (< x 50)
                (> y 450)
                (not info?))
           (change-val w "page" "info")]
          
          [else w])]
       
       ; highscores page
       [hs?
        ; back button
        (if (and (> x 15) (< x 145)
                 (> y 450) (< y 490))
            (change-val w "page" "main")
            w)
        ]
       ; info page
       [info?
        ; back button
        (if (and (> x 15) (< x 145)
                 (> y 450) (< y 490))
            (change-val w "page" "main")
            w)
        ]
       ; settings menu
       [settings?
        (cond 
          ; back button
          [(and (> x 15) (< x 145)
                (> y 450) (< y 490))
           (change-val w "page" "main")]
          
          ; more settings button
          [(and (> x 355) (< x 485)
                (> y 450) (< y 490))
           (change-val w "page" "more")]
          
          ; click on name to clear it
          [(and (> x 150) (< x 490)
                (> y 10) (< y 60))
           (define new-w (change-val w "name" ""))
           (change-val new-w "sb" "name")]
          
          ; click on a key binding to change it
          [(and (> x 150) (< x 350)
                (> y 120) (< y 320))
           (define tl? (and (> x 150) (< x 250) 
                            (> y 120) (< y 220)))
           (define tr? (and (> x 250) (< x 350) 
                            (> y 120) (< y 220)))
           (define bl? (and (> x 150) (< x 250) 
                            (> y 220) (< y 320)))
           (define br? (and (> x 250) (< x 350) 
                            (> y 220) (< y 320)))
           (define tl (key-bindings-tl kb))
           (define tr (key-bindings-tr kb))
           (define bl (key-bindings-bl kb))
           (define br (key-bindings-br kb))
           (define new-w
             (change-val w "kb"  ; clicking on a box clears that binding
                         (cond [tl? (make-key-bindings "" tr bl br)]
                               [tr? (make-key-bindings tl "" bl br)]
                               [bl? (make-key-bindings tl tr "" br)]
                               [br? (make-key-bindings tl tr bl "")]
                               [else kb])))
           (change-val new-w "sb"  ; change selected box to that box
                       (cond [tl? "tl"]
                             [tr? "tr"]
                             [bl? "bl"]
                             [br? "br"]
                             [else sb]))]
          
          ; change difficulty
          [(and (> x 10)  (< x 130)
                (> y 115) (< y 315))
           (define easy?   (and (> x 10)  (< x 130)
                                (> y 115) (< y 185)))
           (define medium? (and (> x 10)  (< x 130)
                                (> y 185) (< y 255)))
           (define hard?   (and (> x 10)  (< x 130)
                                (> y 255) (< y 315)))
           (define new-w
             (cond [easy? (change-val w "difficulty" "easy")]
                   [medium? (change-val w "difficulty" "medium")]
                   [hard? (change-val w "difficulty" "hard")]
                   [else w]))
           (change-val new-w "sb" "name")  ; reset selected box to name
           ]
          ; change sounds
          [(and (> x 370) (< x 490)
                (> y 85)  (< y 355))
           (define normal?  (and (> x 370) (< x 490) 
                                 (> y 85)  (< y 155)))
           (define zelda?   (and (> x 370) (< x 490) 
                                 (> y 155) (< y 225)))
           (define mario?   (and (> x 370) (< x 490) 
                                 (> y 225) (< y 295)))
           (define pokemon? (and (> x 370) (< x 490)
                                 (> y 295) (< y 355)))
           
           (define new-w  ; change selected sound
             (cond [normal? (change-val w "sounds" "normal")]
                   [zelda? (change-val w "sounds" "zelda")]
                   [mario? (change-val w "sounds" "mario")]
                   [pokemon? (change-val w "sounds" "pokemon")]
                   [else w]))
           ; in case selected box is currently a key binding box, set
           ; selected box back to default which is the name box
           (change-val new-w "sb" "name")
           ]
          ; if click position is not inside any box or button, set sb to default
          [else (change-val w "sb" "name")])
        ]
       ; more settings page
       [more? 
        (cond 
          ; back button
          [(and (> x 15) (< x 145)
                (> y 450) (< y 490))
           (change-val w "page" "settings")]
          
          ; click on color name box to clear it
          [(and (> x 165) (< x 495)
                (> y 110) (< y 160))
           (change-val  w "cn" "")
           ]
          
          ; change colors
          [(and (> x 5) (< x 150)
                (> y 5) (< y 430))
           (define min 10)  ; left x
           (define max 150) ; right x
           (define (m n) ; find the y pos of a side button
             (round (+ 5 
                       (* (sub1 n) 
                          (/ 425 7)))))
           (define tl? (and (> x min)  (< x max)
                            (> y (m 1)) (< y (m 2))))
           (define tr? (and (> x min)  (< x max)
                            (> y (m 2)) (< y (m 3))))
           (define bl? (and (> x min)  (< x max)
                            (> y (m 3)) (< y (m 4))))
           (define br? (and (> x min)  (< x max)
                            (> y (m 4)) (< y (m 5))))
           (define bg? (and (> x min)  (< x max)
                            (> y (m 5)) (< y (m 6))))
           (define cc? (and (> x min)  (< x max)
                            (> y (m 6)) (< y (m 7))))
           (define uc? (and (> x min)  (< x max)
                            (> y (m 7)) (< y (m 8))))
           ;; selected color is whatever button you clicked
           (define new-sc (cond [tl? "tl"]
                                [tr? "tr"]
                                [bl? "bl"]
                                [br? "br"]
                                [bg? "bg"]
                                [cc? "cc"]
                                [uc? "uc"]
                                [else sc ])) ; else dont change it
           (define new-w (change-val w "sc" new-sc))
           new-w
           (change-val new-w "cn"  ; change color name to be the new color
                       (cond [(and tl? (color tl)) (color tl)]
                             [(and tr? (color tr)) (color tr)]
                             [(and bl? (color bl)) (color bl)]
                             [(and br? (color br)) (color br)]
                             [(and bg? (color bg)) (color bg)]
                             [(and cc? (color cc)) (color cc)]
                             [(and uc? (color uc)) (color uc)]
                             [else ""]))
           ]
          ;; sliders
          [(or (and (> x 200) (< x 240)
                    (> y 230) (< y 485))
               (and (> x 310) (< x 350)
                    (> y 230) (< y 485))
               (and (> x 420) (< x 460)
                    (> y 230) (< y 485)))
           (define r? (and (> x 200) (< x 240)
                           (> y 230) (< y 485)))
           (define g? (and (> x 310) (< x 350)
                           (> y 230) (< y 485)))
           (define b? (and (> x 420) (< x 460)
                           (> y 230) (< y 485)))
           ; current color based on which box is selected
           (define cur-color (cond [(string=? sc "tl") tl]
                                   [(string=? sc "tr") tr]
                                   [(string=? sc "bl") bl]
                                   [(string=? sc "br") br]
                                   [(string=? sc "bg") bg]
                                   [(string=? sc "cc") cc]
                                   [(string=? sc "uc") uc]))
           (define r (send cur-color red))
           (define g (send cur-color green))
           (define b (send cur-color blue))
           ; find color value given a y pos (is between 0 and 255, inclusive)
           (define (color-y)
             (- 485 y))
           (define (change-rgb)
             (make-color  
              (if r? (color-y) r)  ; change the r g or b of the cur-color
              (if g? (color-y) g)  ; based on which slider is being adjusted
              (if b? (color-y) b)))
           
           ;; first change-val
           ;; changes selected color to be the new color using change-rgb
           (define new-w
             (change-val 
              w "mc" 
              (make-main-colors 
               (if (string=? sc "tl") (change-rgb) tl) 
               (if (string=? sc "tr") (change-rgb) tr)
               (if (string=? sc "bl") (change-rgb) bl)
               (if (string=? sc "br") (change-rgb) br)
               (if (string=? sc "bg") (change-rgb) bg)
               (if (string=? sc "cc") (change-rgb) cc)
               (if (string=? sc "uc") (change-rgb) uc))
              ))
           ;; the new value
           (define new-mc (colors-main-colors (settings-colors (world-settings new-w))))
           ;; second change-val
           ;; if the selected color's rgb values are a color in the-color-database
           ;; then change the color name box's value to be the name of the color
           (define new-w-2 
             (change-val 
              new-w "cn" 
              (cond [(and (string=? sc "tl") (color (main-colors-tl new-mc)))
                     (color (main-colors-tl new-mc))]
                    [(and (string=? sc "tr") (color (main-colors-tr new-mc)))
                     (color (main-colors-tr new-mc))]
                    [(and (string=? sc "bl") (color (main-colors-bl new-mc)))
                     (color (main-colors-bl new-mc))]
                    [(and (string=? sc "br") (color (main-colors-br new-mc)))
                     (color (main-colors-br new-mc))]
                    [(and (string=? sc "bg") (color (main-colors-bg new-mc)))
                     (color (main-colors-bg new-mc))]
                    [(and (string=? sc "cc") (color (main-colors-cc new-mc)))
                     (color (main-colors-cc new-mc))]
                    [(and (string=? sc "uc") (color (main-colors-uc new-mc)))
                     (color (main-colors-uc new-mc))]
                    [else ""])
              ))
           (change-val new-w-2 "ss"  
                       (cond [r? "r"]
                             [g? "g"]
                             [b? "b"]
                             [else #f]))
           ]
          ; if click position is not inside any box or slider
          [else w])
        ]
       [else w])]
    [(string=? e "button-up")
     (change-val w "ss" #f)]
    
    [(string=? e "drag")
     (cond [(string? ss)
            (define cur-color (cond [(string=? sc "tl") tl]
                                    [(string=? sc "tr") tr]
                                    [(string=? sc "bl") bl]
                                    [(string=? sc "br") br]
                                    [(string=? sc "bg") bg]
                                    [(string=? sc "cc") cc]
                                    [(string=? sc "uc") uc]))
            (define r (send cur-color red))
            (define g (send cur-color green))
            (define b (send cur-color blue))
            (define r? (string=? ss "r"))
            (define g? (string=? ss "g"))
            (define b? (string=? ss "b"))
            (define (color-y)
              (cond [(< y 231) 255]
                    [(> y 484) 0]
                    [else (- 485 y)]))
            (define (change-rgb)
              (make-color
               (if r? (color-y) r)
               (if g? (color-y) g)
               (if b? (color-y) b)))
            (define new-w (change-val w "mc" 
                                      (make-main-colors 
                                       (if (string=? sc "tl") (change-rgb) tl)
                                       (if (string=? sc "tr") (change-rgb) tr)
                                       (if (string=? sc "bl") (change-rgb) bl)
                                       (if (string=? sc "br") (change-rgb) br)
                                       (if (string=? sc "bg") (change-rgb) bg)
                                       (if (string=? sc "cc") (change-rgb) cc)
                                       (if (string=? sc "uc") (change-rgb) uc))
                                      ))
            (define new-mc (colors-main-colors (settings-colors (world-settings new-w))))
            (change-val  new-w "cn" 
                         (cond [(and (string=? sc "tl") (color (main-colors-tl new-mc)))
                                (color (main-colors-tl new-mc))]
                               [(and (string=? sc "tr") (color (main-colors-tr new-mc)))
                                (color (main-colors-tr new-mc))]
                               [(and (string=? sc "bl") (color (main-colors-bl new-mc)))
                                (color (main-colors-bl new-mc))]
                               [(and (string=? sc "br") (color (main-colors-br new-mc)))
                                (color (main-colors-br new-mc))]
                               [(and (string=? sc "bg") (color (main-colors-bg new-mc)))
                                (color (main-colors-bg new-mc))]
                               [(and (string=? sc "cc") (color (main-colors-cc new-mc)))
                                (color (main-colors-cc new-mc))]
                               [(and (string=? sc "uc") (color (main-colors-uc new-mc)))
                                (color (main-colors-uc new-mc))]
                               [else ""])
                         )
            ]
           [else w])]
    [else w]
    ))

;; w = world
;; p = parameter in settings
;; v = new val for p
(define (change-val w p v)
  (define s (world-settings w))
  (define c (settings-colors s))
  (make-world (world-cc w) (world-uc w) (world-timer w)
              (make-settings 
               (if (string=? p "name") v
                   (settings-name s)) ; else
               (if (string=? p "difficulty") v
                   (settings-difficulty s)) ; else
               (if (string=? p "sounds") v
                   (settings-sounds s)) ; else
               (if (string=? p "kb") v
                   (settings-key-bindings s)) ; else
               (if (string=? p "sb") v
                   (settings-selected-box s)) ; else
               (if (string=? p "page") v
                   (settings-cur-page s)) ; else
               (make-colors 
                (if (string=? p "mc") v
                    (colors-main-colors c))
                (if (string=? p "sc") v
                    (colors-selected-color c))
                (if (string=? p "ss") v
                    (colors-selected-slider c))
                (if (string=? p "s") v
                    (colors-sliders c))
                (if (string=? p "cn") v
                    (colors-color-name c)))
               ) ; else
              ))

(define (start w)
  (define temp (add1 (random 4)))
  (define m (cond [(string=? (settings-difficulty (world-settings w)) "easy") 20]
                  [(string=? (settings-difficulty (world-settings w)) "medium") 15]
                  [(string=? (settings-difficulty (world-settings w)) "hard") 10]))
  (make-world (list temp)
              (world-uc w)
              (make-timer #t 0 0 0 0 m 1 0)
              (world-settings w)))


;;  world -> world
(define (ontick w)
  (define cc (world-cc w))
  (define uc (world-uc w))
  (define t (world-timer w))
  (define playing? (timer-playing? t))
  
  (define c1 (timer-counterGreen t))
  (define c2 (timer-counterRed t))
  (define c3 (timer-counterYellow t))
  (define c4 (timer-counterBlue t))
  (define compC (timer-counterComputer t))
  (define CL (timer-cur-list t))
  (define wait (timer-wait t))
  (define max-wait (+ (* (sub1 (length (world-cc w))) 15) 10))
  (define s (world-settings w))
  (define m (cond [(string=? (settings-difficulty s) "easy") 20]
                  [(string=? (settings-difficulty s) "medium") 15]
                  [(string=? (settings-difficulty s) "hard") 10]))
  (define sounds (settings-sounds s))
  (cond 
    [playing?
     (cond
       ; if any counter is greater than 0
       [(> (apply + (list c1 c2 c3 c4)) 0)
        ; if any counter equals counter max (based on difficulty)
        (cond 
          [(or (= c1 m)
               (= c2 m)
               (= c3 m)
               (= c4 m))
           ; play piano-tone 
           (play-tone c1 c2 c3 c4 m)
           ])
        ; return world with one subtracted from each counter 
        ; using helper function
        (sub c1 c2 c3 c4 w)]
       ; subtract one from wait if it is greater than 0
       [(positive? wait) 
        ; if user has just entered the last note in the sequence right
        ; then play the sounds (assuming sounds != normal)
        (cond [(= wait max-wait) 
               (pstream-play 
                ps (clip (cond [(string=? sounds "zelda")   zelda]
                               [(string=? sounds "mario")   mario]
                               [(string=? sounds "pokemon") pokemon])
                         0                                        ; start
                         (floor(* 44100 (/ (length cc) 4.5)))))]) ; end
        
        (make-world cc uc (make-timer #t 0 0 0 0 compC CL (sub1 wait)) s)]
       
       ; if computer counter reaches min ...
       [(= compC 1)
        (make-world cc uc 
                    (make-timer #t 0 0 0 0 
                                ; ... if it is at the end of the computer colors
                                ; then set compC to 0, else set it back to the
                                ; counter max and start counting down again
                                (if (= CL (length cc)) 0 m)
                                ; ... if it is at the end of the computer colors
                                ; then set cur-list to 0, else add 1 and start
                                ; counting down using the next color in cc
                                (if (= CL (length cc)) 0 (add1 CL))
                                0)
                    s)]
       ; if compC is greater than 1 then subtract 1
       [(> compC 1)
        (cond 
          [(= compC m)
           ; play piano-tone 
           (define v1 (if (= (list-ref cc (sub1 CL)) 1) m 0))
           (define v2 (if (= (list-ref cc (sub1 CL)) 2) m 0))
           (define v3 (if (= (list-ref cc (sub1 CL)) 3) m 0))
           (define v4 (if (= (list-ref cc (sub1 CL)) 4) m 0))
           (play-tone v1 v2 v3 v4 m)])
        (make-world cc uc  ; sub1 from compC
                    (make-timer #t 0 0 0 0 (sub1 compC) CL 0)
                    s)]
       [else w]
       )]
    [else w]))

;;  number number number number number -> null
(define (play-tone c1 c2 c3 c4 m)
  (pstream-queue 
   ps (clip
       (piano-tone (cond [(= c1 m) 76]
                         [(= c2 m) 81]
                         [(= c3 m) 85]
                         [(= c4 m) 88]))
       0 (* 44100 (/ (- m 2) 30)))
   (pstream-current-frame ps)))

;; number number number number world [number] -> world
;; helper function to subtract 1 from each counter that is greater than 0
(define (sub c1 c2 c3 c4 w (cur 1))
  (cond [(and (= cur 1) (> c1 0)) (sub (sub1 c1) c2 c3 c4 w 2)]
        [(and (= cur 2) (> c2 0)) (sub c1 (sub1 c2) c3 c4 w 3)]
        [(and (= cur 3) (> c3 0)) (sub c1 c2 (sub1 c3) c4 w 4)]
        [(and (= cur 4) (> c4 0)) (sub c1 c2 c3 (sub1 c4) w 5)]
        [(< cur 5) (sub c1 c2 c3 c4 w (add1 cur))]
        [else (make-world (world-cc w) (world-uc w) 
                          (make-timer #t c1 c2 c3 c4 
                                      (timer-counterComputer (world-timer w)) 
                                      (timer-cur-list (world-timer w))
                                      (timer-wait (world-timer w))) 
                          (world-settings w))]))

;; world -> world
(define (todraw w)
  
  (pict->bitmap 
   
   (dc (λ (dc dx dy)
         ; set temp vars of old brush,pen,font,foreground
         (define old-brush (send dc get-brush))
         (define old-pen (send dc get-pen))
         (define old-font (send dc get-font))
         (define old-foreground (send dc get-text-foreground))
         
         
         (cond [(string=? (settings-cur-page (world-settings w))
                          "settings")
                (draw-settings w dc)]
               [(string=? (settings-cur-page (world-settings w))
                          "highscores")
                (draw-highscores w dc)]
               [(string=? (settings-cur-page (world-settings w))
                          "info")
                (draw-info w dc)]
               [(string=? (settings-cur-page (world-settings w))
                          "more")
                (draw-more w dc)]
               [(not (timer-playing? (world-timer w)))
                (draw-main w dc dx dy)
                (draw-home w dc)]
               [else 
                (draw-main w dc dx dy)
                (draw-cur-score w dc)])
         
         ; reset old brush,pen,font,foreground
         (send dc set-brush old-brush)
         (send dc set-pen old-pen)
         (send dc set-font old-font)
         (send dc set-text-foreground old-foreground)
         )
       ;; size
       500 500)
   )
  )

;; world drawing-context% -> drawing-context%
(define (draw-info w dc)
  ; info text
  (send dc set-font (make-object font% 32 "Ariel" 'default))
  (send dc draw-text "Hit the keys corresponding with" 20 20)
  (send dc draw-text "the correct color in the pattern." 20 70)
  (send dc draw-text "The default keys are 1, 2, 3 and" 20 120)
  (send dc draw-text "4. Go to settings to change the" 20 170)
  (send dc draw-text "name, key bindings, difficulty," 20 220)
  (send dc draw-text "sounds and colors" 20 270)
  
  (send dc set-font (make-font #:size 40 #:face "Times New Roman" #:weight 'bold))
  (send dc set-text-foreground "firebrick")
  (send dc draw-text "Team FireBrick" 116 360)
  (send dc set-text-foreground "black")
  ; back button
  (draw-back dc)
  )

;; world drawing-context% -> drawing-context%
(define (draw-settings w dc)
  ; key bindings variables
  (define tl (key-bindings-tl (settings-key-bindings (world-settings w))))
  (define tr (key-bindings-tr (settings-key-bindings (world-settings w))))
  (define bl (key-bindings-bl (settings-key-bindings (world-settings w))))
  (define br (key-bindings-br (settings-key-bindings (world-settings w))))
  ; difficulty and sound settings
  (define easy? (string=? (settings-difficulty (world-settings w)) "easy"))
  (define medium? (string=? (settings-difficulty (world-settings w)) "medium"))
  (define hard? (string=? (settings-difficulty (world-settings w)) "hard"))
  (define normal? (string=? (settings-sounds (world-settings w)) "normal"))
  (define zelda? (string=? (settings-sounds (world-settings w)) "zelda"))
  (define mario? (string=? (settings-sounds (world-settings w)) "mario"))
  (define pokemon? (string=? (settings-sounds (world-settings w)) "pokemon"))
  
  ; name box
  (send dc set-font (make-object font% 25 "Ariel" 'default))
  (send dc draw-text "Enter Name:" 5 24)
  (send dc draw-rectangle 150 10 345 50)
  (send dc set-font (make-object font% 35 "Times New Roman" 'default))
  (send dc draw-text (settings-name (world-settings w)) 158 18)
  
  ; back button and more button
  (draw-back dc)
  (send dc draw-rounded-rectangle 355 450 130 40 10)
  (send dc set-font (make-object font% 46 "Ariel" 'default))
  (send dc draw-text "→" 445 434)
  (send dc set-font (make-object font% 30 "Ariel" 'default))
  (send dc draw-text "More" 370 457)
  
  (send dc draw-text "Click inside a box to change it" 50 390)
  
  ; key binding boxes
  (define KBY 135)
  (define KBX 150)
  (send dc draw-text "Key-Bindings" 160 98)
  (send dc draw-rectangle KBX         KBY         (+ 50 KBX)  (+ 65 KBY))
  (send dc draw-line      (+ 100 KBX) KBY         (+ 100 KBX) (+ 200 KBY))
  (send dc draw-line      KBX         (+ 100 KBY) (+ 200 KBX) (+ 100 KBY))
  
  ; key binding keys
  (send dc set-font (make-object font% 35 "Ariel" 'default))
  (send dc draw-text tl (+ 40 KBX)  (+ 35 KBY))
  (send dc draw-text tr (+ 140 KBX) (+ 35 KBY))
  (send dc draw-text bl (+ 40 KBX)  (+ 135 KBY))
  (send dc draw-text br (+ 140 KBX) (+ 135 KBY))
  
  ; draw difficulty and sound buttons
  (define WIDTH 120)
  (define HEIGHT 60)
  (send dc draw-rounded-rectangle 10  120 WIDTH HEIGHT)
  (send dc draw-rounded-rectangle 10  190 WIDTH HEIGHT)
  (send dc draw-rounded-rectangle 10  260 WIDTH HEIGHT)
  (send dc draw-rounded-rectangle 370 90  WIDTH HEIGHT)
  (send dc draw-rounded-rectangle 370 160 WIDTH HEIGHT)
  (send dc draw-rounded-rectangle 370 230 WIDTH HEIGHT)
  (send dc draw-rounded-rectangle 370 300 WIDTH HEIGHT)
  
  ; highlight selected box in red
  (send dc set-pen
        (new pen% 
             [width 3] 
             [color "red"])) 
  (cond [easy?    (send dc draw-rounded-rectangle 10  120 WIDTH HEIGHT)]
        [medium?  (send dc draw-rounded-rectangle 10  190 WIDTH HEIGHT)]
        [hard?    (send dc draw-rounded-rectangle 10  260 WIDTH HEIGHT)])
  (cond [normal?  (send dc draw-rounded-rectangle 370 90  WIDTH HEIGHT)]
        [zelda?   (send dc draw-rounded-rectangle 370 160 WIDTH HEIGHT)]
        [mario?   (send dc draw-rounded-rectangle 370 230 WIDTH HEIGHT)]
        [pokemon? (send dc draw-rounded-rectangle 370 300 WIDTH HEIGHT)])
  
  ; draw difficulty and sound text
  (send dc set-font (make-object font% 28 "Times New Roman" 'default))
  (send dc draw-text "Easy" 42 137)
  (send dc draw-text "Medium" 21 207)
  (send dc draw-text "Hard" 40 277)
  (send dc draw-text "Normal" 389 107)
  (send dc draw-text "Zelda" 399 177)
  (send dc draw-text "Mario" 397 247)
  (send dc draw-text "Pokemon" 377 317)
  )

;; world drawing-context% -> drawing-context%
(define (draw-more w dc)
  ; color variables
  (define colors (settings-colors (world-settings w)))
  
  (define cn (colors-color-name colors))
  (define tl (main-colors-tl (colors-main-colors colors)))
  (define tr (main-colors-tr (colors-main-colors colors)))
  (define bl (main-colors-bl (colors-main-colors colors)))
  (define br (main-colors-br (colors-main-colors colors)))
  (define bg (main-colors-bg (colors-main-colors colors)))
  (define cc (main-colors-cc (colors-main-colors colors)))
  (define uc (main-colors-uc (colors-main-colors colors)))
  
  (define tl? (string=? (colors-selected-color colors) "tl"))
  (define tr? (string=? (colors-selected-color colors) "tr"))
  (define bl? (string=? (colors-selected-color colors) "bl"))
  (define br? (string=? (colors-selected-color colors) "br"))
  (define bg? (string=? (colors-selected-color colors) "bg"))
  (define cc? (string=? (colors-selected-color colors) "cc"))
  (define uc? (string=? (colors-selected-color colors) "uc"))
  
  (define cur-color (cond [tl? tl] [tr? tr] [bl? bl] [br? br]
                          [bg? bg] [cc? cc] [uc? uc]))
  
  ; color name box
  (send dc set-font (make-object font% 25 "Ariel" 'default))
  (send dc draw-text "Enter Color Name:" 220 75)
  (send dc draw-rectangle 165 110 330 50)
  (send dc draw-text cn 170 120)
  (send dc draw-text "Or Drag the Sliders:" 205 175)
  
  ; back button and more button
  (draw-back dc)
  
  (send dc set-font (make-object font% 28 "Times New Roman" 'default))
  (send dc draw-text "← Select a color to change it" 160 10)
  
  
  
  ; draw color buttons
  (define WIDTH 140)
  (define HEIGHT 53)
  ;; number -> number
  ;; returns Y pos of each rectangle
  (define (get-pos n)
    (round (+ (* n 8) (* HEIGHT (sub1 n)))))
  (define X 10)
  (define (change-brush c)
    (send dc set-brush (new brush% [style 'solid] [color c])))
  (send dc set-pen (new pen% [width 2] [color "black"]))
  (change-brush tl)
  (send dc draw-rounded-rectangle X (get-pos 1) WIDTH HEIGHT)
  (change-brush tr)
  (send dc draw-rounded-rectangle X (get-pos 2) WIDTH HEIGHT)
  (change-brush bl)
  (send dc draw-rounded-rectangle X (get-pos 3) WIDTH HEIGHT)
  (change-brush br)
  (send dc draw-rounded-rectangle X (get-pos 4) WIDTH HEIGHT)
  (change-brush bg)
  (send dc draw-rounded-rectangle X (get-pos 5) WIDTH HEIGHT)
  (change-brush cc)
  (send dc draw-rounded-rectangle X (get-pos 6) WIDTH HEIGHT)
  (change-brush uc)
  (send dc draw-rounded-rectangle X (get-pos 7) WIDTH HEIGHT)
  (send dc set-brush (new brush% [style 'transparent] [color "black"]))
  
  
  ; highlight selected box in red
  (send dc set-pen (new pen% [width 3] [color "red"])) 
  (cond [tl? (send dc draw-rounded-rectangle X (get-pos 1) WIDTH HEIGHT)]
        [tr? (send dc draw-rounded-rectangle X (get-pos 2) WIDTH HEIGHT)]
        [bl? (send dc draw-rounded-rectangle X (get-pos 3) WIDTH HEIGHT)]
        [br? (send dc draw-rounded-rectangle X (get-pos 4) WIDTH HEIGHT)]
        [bg? (send dc draw-rounded-rectangle X (get-pos 5) WIDTH HEIGHT)]
        [cc? (send dc draw-rounded-rectangle X (get-pos 6) WIDTH HEIGHT)]
        [uc? (send dc draw-rounded-rectangle X (get-pos 7) WIDTH HEIGHT)])
  
  (define (get-text-pos n)
    (+ (get-pos n) 13))
  
  ; if combined rgb values > 250, draw text in black
  ; else draw text in white
  (define (black-or-white c)
    (if (> (+ (send c red) (send c green) (send c blue)) 250)
        (send dc set-text-foreground "black")
        (send dc set-text-foreground "white")))
  
  ; draw colors boxes text
  (send dc set-font (make-font #:size 19 #:face "Times New Roman" #:weight 'bold))
  (black-or-white tl)
  (send dc draw-text "Top Left"       43 (get-text-pos 1))
  (black-or-white tr)
  (send dc draw-text "Top Right"      37 (get-text-pos 2))
  (black-or-white bl)
  (send dc draw-text "Bottom Left"    29 (get-text-pos 3))
  (black-or-white br)
  (send dc draw-text "Bottom Right"   23 (get-text-pos 4))
  (black-or-white bg)
  (send dc draw-text "Background"     27 (get-text-pos 5))
  (black-or-white cc)
  (send dc draw-text "Computer Color" 11 (get-text-pos 6))
  (black-or-white uc)
  (send dc draw-text "User Color"     33 (get-text-pos 7))
  
  ;; draw sliders
  (send dc draw-line 220 230 220 485)
  (send dc draw-line 330 230 330 485)
  (send dc draw-line 440 230 440 485)
  
  (define r (send cur-color red))
  (define g (send cur-color green))
  (define b (send cur-color blue))
  
  (send dc set-pen (new pen% [width 2] [color "black"]))
  (send dc set-brush (new brush% [style 'solid] [color "white"]))
  (send dc draw-rectangle 200 (- 472 r) 40 25) ; r slider button
  (send dc draw-rectangle 310 (- 472 g) 40 25) ; g slider button
  (send dc draw-rectangle 420 (- 472 b) 40 25) ; b slider button
  
  (define (get-base-x n)
    (if (not (positive? n)) 15
        (+ 5 (* 5 (- 2 (floor (log n 10)))))))
  
  (send dc draw-text (number->string r) ; r slider
        (+ 200 (get-base-x r))
        (- 472 r))
  (send dc draw-text (number->string g) ; g slider
        (+ 310 (get-base-x g))
        (- 472 g))
  (send dc draw-text (number->string b) ; b slider
        (+ 420 (get-base-x b))
        (- 472 b))
  )


(define (draw-highscores w dc)
  (define my-table (list (list "score" make-score)))
  (define in (open-input-file "scores.txt"))
  (define scores
    (string->struct/maker my-table
                          (port->string in)))
  (define text-height 40)
  (define text-starting-height 50)
  (send dc set-font (make-object font% 28 "Ariel" 'default))
  (send dc draw-text 
        "Name"
        120 10
        )
  ; draw names
  (map (lambda (i)
         (send dc  draw-text 
               ; name
               (string-append (score-name (list-ref scores i)) "\n")
               120                   ;x
               (+ text-starting-height
                  (* text-height i)) ;y
               ))
       (range 0 (min 10 (length scores)) 1))
  (send dc draw-text 
        "Score"
        320 10
        )
  ; draw scores
  (map (lambda (i)
         (send dc  draw-text 
               ; score
               (string-append (number->string (score-score (list-ref scores i))) "\n")
               320                   ;x
               (+ text-starting-height 
                  (* text-height i)) ;y
               ))
       (range 0 (min 10 (length scores)) 1))
  (draw-back dc)
  )

; draw circle with colors
(define (draw-main w dc dx dy)
  (define red (new dc-path%))
  (send red move-to 250 250)
  (send red line-to 500 250)
  (send red curve-to 500 112.5 387.5 0 250 0)
  (send red line-to 250 250)
  (send red close)
  (define green (new dc-path%))
  (send green move-to 250 250)
  (send green line-to 250 0)
  (send green curve-to 112.5 0 0 112.5 0 250)
  (send green line-to 250 250)
  (send green close)
  (define yellow (new dc-path%))
  (send yellow move-to 250 250)
  (send yellow line-to 0 250)
  (send yellow curve-to 0 387.5 112.5 500 250 500)
  (send yellow line-to 250 250)
  (send yellow close)
  (define blue (new dc-path%))
  (send blue move-to 250 250)
  (send blue line-to 250 500)
  (send blue curve-to 387.5 500 500 387.5 500 250)
  (send blue line-to 250 250)
  (send blue close)
  
  
  
  (define computer-colors (world-cc w))
  (define t (world-timer w))
  (define c1 (timer-counterGreen t))
  (define c2 (timer-counterRed t))
  (define c3 (timer-counterYellow t))
  (define c4 (timer-counterBlue t))
  (define compC (timer-counterComputer t))
  (define CL (timer-cur-list t))
  (define wait (timer-wait t))
  
  (define comp-color (if (and (> compC 2) 
                              (= (+ c1 c2 c3 c4) 0)
                              (= wait 0)) 
                         (list-ref computer-colors (sub1 CL)) 0))
  (define green?  (or (= comp-color 1) 
                      (> c1 0)))
  (define red?    (or (= comp-color 2) 
                      (> c2 0)))
  (define yellow? (or (= comp-color 3) 
                      (> c3 0)))
  (define blue?   (or (= comp-color 4) 
                      (> c4 0)))
  
  ; color definitions
  (define mc (colors-main-colors (settings-colors (world-settings w))))
  (define tl (main-colors-tl mc))
  (define tr (main-colors-tr mc))
  (define bl (main-colors-bl mc))
  (define br (main-colors-br mc))
  (define bg (main-colors-bg mc))
  (define cc (main-colors-cc mc))
  (define uc (main-colors-uc mc))
  
  (define (pink-or-black n) 
    (if (or (and (> c1 0) (= n 1)) 
            (and (> c2 0) (= n 2)) 
            (and (> c3 0) (= n 3))
            (and (> c4 0) (= n 4))) uc cc))
  
  ; draw background
  (send dc set-pen (new pen% [style 'transparent]))
  (send dc set-brush (new brush%
                          [style 'solid]
                          [color bg]))
  (send dc draw-rectangle -10 -10 510 510)
  
  (send dc set-pen
        (new pen% 
             [width 3] 
             [color "black"])) 
  
  ;green
  (send dc set-brush (new brush% 
                          [style 'solid]
                          [color (if green?  (pink-or-black 1) tl)]))
  (send dc draw-path green dx dy)
  ;red
  (send dc set-brush (new brush% 
                          [style 'solid]
                          [color (if red?    (pink-or-black 2) tr)]))
  (send dc draw-path red dx dy)
  ;yellow
  (send dc set-brush (new brush% 
                          [style 'solid]
                          [color (if yellow? (pink-or-black 3) bl)]))
  (send dc draw-path yellow dx dy)
  ;blue
  (send dc set-brush (new brush% 
                          [style 'solid]
                          [color (if blue?   (pink-or-black 4) br)]))
  (send dc draw-path blue dx dy)
  
  (send dc set-brush
        (new brush% 
             [style 'transparent]))
  (send dc set-pen
        (new pen% 
             [width 5] 
             [color "black"]))
  (send dc draw-ellipse 0 0 500 500))



(define (draw-home w dc)
  
  (define tl (key-bindings-tl (settings-key-bindings (world-settings w))))
  (define tr (key-bindings-tr (settings-key-bindings (world-settings w))))
  (define bl (key-bindings-bl (settings-key-bindings (world-settings w))))
  (define br (key-bindings-br (settings-key-bindings (world-settings w))))
  
  (send dc set-brush
        (new brush% 
             [style 'solid]
             [color "LightGray"]))
  (send dc draw-rounded-rectangle 100 200 300 100 50)
  (send dc set-font (make-object font% 80 "Courier" 'default))
  (send dc draw-text "START" 130 214)
  (send dc set-font (make-object font% 50 "Ariel" 'default))
  (send dc draw-text tr 350 100)
  (send dc draw-text tl 120 100)
  (send dc draw-text bl 120 350)
  (send dc draw-text br 350 350)
  (send dc draw-text "⚙" 460 460)
  (send dc set-font (make-object font% 30 "Times New Roman, italic" 'default))
  (send dc draw-text "i" 19 460)
  (send dc draw-bitmap (pict->bitmap (scale (bitmap "podium.png") 0.065)) 455 13) 
  (send dc set-pen
        (new pen% 
             [width 2] 
             [color "black"]))
  (send dc set-brush
        (new brush% 
             [style 'transparent]
             ))
  
  (send dc draw-ellipse 8 462 33 33))

(define (draw-cur-score w dc)
  (send dc set-font (make-object font% 40 "Courier New" 'default))
  (send dc draw-text (number->string (sub1 (length (world-cc w))))
        ;; x moves left by 30 if score >= 10 to make it stay inside the window
        (- 500 (+ 40 (* 30 (floor (/ (sub1 (length (world-cc w))) 10))))) 
        450))

(define (draw-back dc)
  (send dc draw-rounded-rectangle 15 450 130 40 10)
  (send dc set-font (make-object font% 46 "Ariel" 'default))
  (send dc draw-text "←" 25 434)
  (send dc set-font (make-object font% 30 "Ariel" 'default))
  (send dc draw-text "Back" 60 457))

;; Big Bang

(big-bang default-world 
          (to-draw todraw)
          (on-tick ontick 1/45)
          (on-key onkey)
          (on-mouse onmouse)
          )



; test cases


;; check-for-color
(check-equal?
 (check-for-color color-names (send the-color-database find-color "slateblue"))
 "slate blue")
(check-equal?
 (check-for-color color-names (make-color 3 4 5))
 #f)

;; equal-rgb?
(check-equal? 
 (equal-rgb? (make-color 255 0 0)
             (send the-color-database find-color "red"))
 #t)

;; right-so-far?
(check-equal? 
 (right-so-far? (list) (list))
 #t)
(check-equal? 
 (right-so-far? (list 1 2 3) (list 4 3 2))
 #f)
