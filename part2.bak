;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname part2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;;===========================================
;; PART 2
;; Encapsulated template
(define (fn-for-widget w)
  (local [(define (fn-for-widget--widget w)
            (...(widget-name w)
                (widget-quanity w)
                (widget-time w)
                (widget-price w)
                (fn-for-widget--low (widget-parts w))))
          (define (fn-for-widget--low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-widget--widget (first low))
                        (fn-for-widget--low (rest low)))]))]
    (fn-for-widget--widget w)))

;; Widget Natural Number -> (listof Widget)
;; Consumes widget, time, and cost, then returns list of widget and subwidgets that have a time
;; less than t (time) and price greater than cost
(check-expect (find-widgets-good-make Wire 10 1) (list Wire))
(check-expect (find-widgets-good-make Wire 10 10) empty)
(check-expect (find-widgets-good-make Wire 1 1) empty)
(check-expect (find-widgets-good-make Wire 1 10) empty)
(check-expect (find-widgets-good-make Cord 10 1) (list Cord Wire))
(check-expect (find-widgets-good-make Cord 6 1) (list Wire))
(check-expect (find-widget-hard-make (widget-parts Cord) 10 1) (list Wire))

(define (find-widgets-good-make w t cost) empty) ;stub

;; Widget Natural Natural -> (listof Widget)
;; Consumes a widget, a name length, and a number of parts, then returns a list of widget and
;; subwidgets with a name longer than the length and with a num parts greater than parts
(check-expect (find-widgets-tough-advertise Wire 10 10) (list Wire))
(check-expect (find-widgets-tough-advertise Wire 1 10) empty)
(check-expect (find-widgets-tough-advertise Wire 10 0) empty)
(check-expect (find-widgets-tough-advertise Wire 1 0) empty)
(check-expect (find-widgets-tough-advertise Cord 10 3) (list Cord Wire))
(check-expect (find-widgets-tough-advertise Cord 1 1) (list Wire))
(check-expect (find-widgets-tough-advertise Jewlery 10 4) (list Jewelry Ring Necklace Bracelet))
(check-expect (find-widgets-tough-advertise Jewlery 5 4) (list Ring))

(define (find-widgets-tough-advertise length parts) empty)

;; Widget (Widget -> Boolean) Y Y -> (listof Widget)
;; Consumes a widget, function which produces a boolean, and two other parameters, then returns a list
;; of widgets and subwidgets which cause the fn to return true

(define (fn-for-widget w fn cond1 cond2)
  (local [(define (fn-for-widget--widget w)
            (...(widget-name w)
                (widget-quanity w)
                (widget-time w)
                (widget-price w)
                (fn-for-widget--low (widget-parts w))))
          (define (fn-for-widget--low low)
            (cond [(empty? low) (...)]
                  [else
                   (... (fn-for-widget--widget (first low))
                        (fn-for-widget--low (rest low)))]))]
    (fn-for-widget--widget w)))
