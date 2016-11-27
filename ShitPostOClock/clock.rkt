#lang racket/gui
(require racket/date)


(define bitmap (read-bitmap "clocks/01/00.jpg"))

;(define (is-pepe?)
;  (if (> (random) 0.55) #t #f))
(define (is-pepe?) #t)


(define (get-current-year)
  (let ((date (seconds->date (current-seconds))))
    (date-year date)))
(define (is-leapyear?)
  (apply (lambda (a b c) (or a (and (not b) c)))
       (map (lambda (m) (zero? (remainder (get-current-year) m)))
            '(400 100 4))))

(define (get-current-month)
  (let ((date (seconds->date (current-seconds))))
    (date-month date)))
(define (get-current-hour)
  (let ((date (seconds->date (current-seconds))))
    (date-hour date)))
(define (get-current-minute)
   (let ((date (seconds->date (current-seconds))))
    (date-minute date)))

(define (get-current-day)
  (let ((date (seconds->date (current-seconds))))
    (date-day date)))

(define (get-julian-day)
  (cond
    [(= 1 (get-current-month)) (get-current-day)]
    [(= 2 (get-current-month)) (+ (get-current-day) 31)]
    [(= 3 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29)
                                   (+ (get-current-day) 31 28))]
    [(= 4 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31)
                                   (+ (get-current-day) 31 28 31))]
    [(= 5 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31 30)
                                   (+ (get-current-day) 31 28 31 30))]
    [(= 6 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31 30 31)
                                   (+ (get-current-day) 31 28 31 30 31))]
    [(= 7 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31 30 31 30)
                                   (+ (get-current-day) 31 28 31 30 31 30))]
    [(= 8 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31 30 31 30 31)
                                   (+ (get-current-day) 31 28 31 30 31 30 31))]
    [(= 9 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31 30 31 30 31 31)
                                   (+ (get-current-day) 31 28 31 30 31 30 31 31))]
    [(= 10 (get-current-month)) (if (is-leapyear?)
                                    (+ (get-current-day) 31 29 31 30 31 30 31 31 30)
                                   (+ (get-current-day) 31 28 31 30 31 30 31 31 30))]
    [(= 11 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31 30 31 30 31 31 30 31)
                                   (+ (get-current-day) 31 28 31 30 31 30 31 31 30 31))]
    [(= 12 (get-current-month)) (if (is-leapyear?)
                                   (+ (get-current-day) 31 29 31 30 31 30 31 31 30 31 30)
                                   (+ (get-current-day) 31 28 31 30 31 30 31 31 30 31 30))]))

(define (determine-day-offset)
  (cond [(= 2016 (get-current-year))
         (- (+ (truncate (/ (get-julian-day) 57))
               (quotient (get-julian-day) 57)) 1)]))

(define (determine-minutes-behind)
  (cond [(= 0 (determine-day-offset))
         (* (get-current-hour) 5)]
        [else (+ (* (determine-day-offset) 120)
                 (* (get-current-hour) 5))]))

(define (minutes-since-reset)
  (- (+ (* (determine-day-offset) 24 60)
        (* (get-current-hour) 60)
        (get-current-minute))
     (determine-minutes-behind)))

(define (remove-days)
  (- (minutes-since-reset)
     (* (truncate (/ (minutes-since-reset)
                     (* 60 24)))
        60 24)))

(define (get-current-base13-hour)
  (truncate (/ (remove-days) 65)))

(define (get-current-base13-hour-12h)
  (if (> (get-current-base13-hour) 13)
      (- (get-current-base13-hour) 13)
      (get-current-base13-hour)))

(define (get-current-base13-minute)
  (- (remove-days) (* (get-current-base13-hour) 65)))

(define f (new frame% [label "Clock"]
                      [width 832]
                      [height 576]))
(new message% [parent f] [label bitmap])

(define (main)
  (begin
    
    
    (send f show #f)
    (set! f (new frame% [label "Clock"]
                        [width 832]
                        [height 576]))
    (if  (is-pepe?)
         (begin
           (set! bitmap (read-bitmap (string-append "pepe/"
                                                    (number->string (truncate (+ (* (random) 9) 1)))
                                                    ".png")))
           (new message% [parent f] [label bitmap])
           (send f show #t))
         (begin
           (set! bitmap (read-bitmap (string-append "clocks/"
                                                    (number->string  (get-current-base13-hour-12h))
                                                    "/"
                                                    (number->string  (get-current-base13-minute))
                                                    ".jpg")))
           (new message% [parent f] [label bitmap])
           (send f show #t)))

    
    
    (sleep 1)
    (main)))

;;; MAIN PROGRAM
;(main)
(display (get-current-base13-hour-12h))
(display ":")
(get-current-base13-minute)


 

