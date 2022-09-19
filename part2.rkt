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
;;===========================================
;; PART 2
;; Encapsulated template
#;
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

;(define (find-widgets-good-make w t cost) empty) ;stub

;; For "Third"
(define (find-widgets-good-make w t cost)
  (local [(define (good? w)
            (and (< (widget-time w) t) (> (widget-price w) cost)))]
    (fn-for-widget w good?)))
    

;; Widget Natural Natural -> (listof Widget)
;; Consumes a widget, a name length, and a number of parts, then returns a list of widget and
;; subwidgets with a name longer than the char and with a num parts greater than parts
(check-expect (find-widgets-tough-advertise Wire 1 0) empty)
(check-expect (find-widgets-tough-advertise Cord 1 10) empty)
(check-expect (find-widgets-tough-advertise Wire 10 10) empty)
(check-expect (find-widgets-tough-advertise Cord 1 0) (list Cord))
(check-expect (find-widgets-tough-advertise Jewelry 6 0)
              (list Jewelry Necklace Bracelet))
(check-expect (find-widgets-tough-advertise Jewelry 1 0)
              (list Jewelry Necklace Bracelet Beads))

;(define (find-widgets-tough-advertise char parts) empty)


(define (find-widgets-tough-advertise w char parts)
  (local [(define (tough? w)
            (and (> (string-length (widget-name w)) char)
                 (> (num-parts w) parts)))
          (define (num-parts w)
            (- (length (fn-for-widget w (λ (w) (not (empty? w))))) 1))]
    ;;Subtract 1 from length of list because the widget does not count as a part
    (fn-for-widget w tough?)))

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


;; Widget Natural -> ListOfWidget
;; ListOfWidget Natural -> ListOfWidget
;; Examines the widget and sub widgets, and returns those with a name of length longer
;; than inputted natural
(check-expect (find-widget-name-longer-than Wire 2) (list Wire))
(check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))
(check-expect (find-widget-name-longer-than Numbers 20) empty)

(define (find-widget-name-longer-than w length)
  (local [(define (longer-than? w)
            (> (string-length (widget-name w)) length))]
    (fn-for-widget w longer-than?)))

;; 2.
;; Widget Natural -> ListOfWidget
;; Returns widgets and subwidgets whose quantities are greater than the inputted natural
(check-expect (find-widget-quantity-over Wire 2) (list Wire))
(check-expect (find-widget-quantity-over Cord 2) (list Cord Wire))
(check-expect (find-widget-quantity-over Cord 10) empty)

(define (find-widget-quantity-over w quan)
  (local [(define (more-than? w)
            (> (widget-quantity w) quan))]
    (fn-for-widget w more-than?)))

;; 3.
;; Widget Number -> (listof Widget)
;; Consumes a widget and a number, then returns widget and subwidgets which have a price less than num
(check-expect (find-widgets-cheaper-than Wire 10) (list Wire))
(check-expect (find-widgets-cheaper-than Cord 10) (list Cord Wire))
(check-expect (find-widgets-cheaper-than Cord 1) empty)

(define (find-widgets-cheaper-than w num)
  (local [(define (cheaper-than? w)
            (< (widget-price w) num))]
    (fn-for-widget w cheaper-than?)))

;; 4.
;; Widget Natural Number -> (listof Widget)
;; Consumes a widet, quantity, and price, then returns list of widget and sub widgets that have a
;; quantity less than quan, or has a price greater than cost
(check-expect (find-widget-hard-make Wire 10 1) (list Wire))
(check-expect (find-widget-hard-make Wire 10 10) (list Wire))
(check-expect (find-widget-hard-make Wire 7 4) (list Wire))
(check-expect (find-widget-hard-make Wire 1 10) empty)
(check-expect (find-widget-hard-make Cord 10 1) (list Cord Wire))
(check-expect (find-widget-hard-make Cord 5 10) (list Wire))

(define (find-widget-hard-make w quan cost)
  (local [(define (difficult? w)
            (or (> (widget-price w) cost) (< (widget-quantity w) quan)))]
    (fn-for-widget w difficult?)))








