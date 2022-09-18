;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Jacob Murphy and Leo Strangman
(define-struct widget(name quantity time price parts))
;; a widget is a (make-widget String Natural Natural Number (listof widget))

;; template
#;
(define (fn-for-widget--widget w)
  (...(widget-name w)
      (widget-quanity w)
      (widget-time w)
      (widget-price w)
      (fn-for-widget--low (widget-parts w))))
#;
(define (fn-for-widget--low low)
  (cond [(empty? low) (...)]
        [else
         (... (fn-for-widget--widget (first low))
              (fn-for-widget--low (rest low)))]))

;; here is one hierarchy of Widgets used to make a telephone
(define Wire (make-widget "Wire" 3 5 5 empty))
(define Cord (make-widget "Cord" 9 8 5 (list Wire)))
(define Numbers (make-widget "Numbers" 9 5 5 empty))
(define Buttons (make-widget "Buttons" 8 5 5 (list Numbers)))
(define Receiver (make-widget "Receiver" 10 5 7 empty))
(define Telephone (make-widget "Telephone" 5 20 15
                               (list Receiver Buttons Cord)))

;; here is a second hierarchy of Widgets used to make a set of jewelery
(define Glass (make-widget "Glass" 6 9 4 empty))
(define Beads (make-widget "Beads" 25 12 7 (list Glass)))
(define Bracelet (make-widget "Bracelet" 5 3 5 (list Beads)))
(define Chain (make-widget "Chain" 7 2 1 empty))
(define Pendant (make-widget "Pendant" 4 3 1 empty))
(define Necklace (make-widget "Necklace" 10 7 3
                              (list Chain Pendant)))
(define Ring (make-widget "Ring" 15 8 10 empty))
(define Jewelry (make-widget "Jewelry set" 4 17 30
                             (list Ring Necklace Bracelet)))

;; Widget (Widget -> Boolean) -> (listof Widget)
;; Consumes a widget and a function which consumes a widget and produces a boolean, then returns a 
;; list of widgets and subwidgets which cause the fn to return true
(check-expect (fn-for-widget Wire (λ (w) (> (widget-price w) 1))) (list Wire))

(define (fn-for-widget w fn?)
  (local [(define (fn-for-widget--widget w)
            (if (fn? w)
             (cons w (fn-for-widget--low (widget-parts w)))
                   (fn-for-widget--low (widget-parts w))))
          (define (fn-for-widget--low low)
            (cond [(empty? low) empty]
                  [else
                   (append (fn-for-widget--widget (first low))
                        (fn-for-widget--low (rest low)))]))]
    (fn-for-widget--widget w)))


;================================================
;Part 3


;Signature: (X X -> Boolean) X -> ((listof X) -> (listof X))
;Purpose: Sorts given field of widget based on comparison function fn?
 

(define (qsort w fn? field)
  (local
    [(define (make-low w)
       (fn-for-widget w (λ (w) (not (empty? w)))))
     (define low (make-low w))
     (define (qsort-list low)
       (cond
         [(empty? low) empty]
         [else 
          (local
            [(define pivot (first low))
             (define (smaller? x)
               (fn? (field x) (field pivot)))] ;; create a closure
            (append
             (qsort-list (filter smaller? (rest low)))
             (list pivot)
             (qsort-list (filter (λ(x) 
                              (not (smaller? x)))
                            ;; ^^^ invert the smaller? function
                            (rest low)))))]))]
    (qsort-list (make-low w))))
(define sort-strings (qsort Telephone string<? widget-name))
