;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Widget Natural -> ListOfWidget
;; ListOfWidget Natural -> ListOfWidget
;; Examines the widget and sub widgets, and returns those with a name of length longer than inputted natural
(check-expect (find-widget-name-longer-than Wire 2) (list Wire))
(check-expect (find-widget-name-longer-than Cord 2) (list Cord Wire))
(check-expect (find-widget-name-longer-than Numbers 20) empty)
(check-expect (find-widget-name-longer-than--low (widget-parts Cord) 2) (list Wire))

;(define (find-widget-name-longer-than low length) empty) ;stub
;(define (find-widget-name-longer-than--low low length) empty)

(define (find-widget-name-longer-than w length)
  (if (> (string-length (widget-name w)) length)
      (cons w 
      (find-widget-name-longer-than--low (widget-parts w) length))
      (find-widget-name-longer-than--low (widget-parts w) length)))

(define (find-widget-name-longer-than--low low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length)
              (find-widget-name-longer-than--low (rest low) length))]))

;; Widget Natural -> ListOfWidget
;; Returns widgets and subwidgets whose quantities are greater than the inputted natural
(check-expect (find-widget-quantity-over Wire 2) (list Wire))
(check-expect (find-widget-quantity-over Cord 2) (list Cord Wire))
(check-expect (find-widget-quantity-over Cord 10) empty)
(check-expect (find-widget-quantity-over--low (widget-parts Cord) 1) (list Wire))
              
              
;(define (find-widget-quantity-over w quan) empty)
;(define (find-widget-quantity-over--low low quan) empty)  

(define (find-widget-name-longer-than w length)
  (if (> (string-length (widget-name w)) length)
      (cons w 
      (find-widget-name-longer-than--low (widget-parts w) length))
      (find-widget-name-longer-than--low (widget-parts w) length)))

(define (find-widget-name-longer-than--low low length)
  (cond [(empty? low) empty]
        [else
         (append (find-widget-name-longer-than (first low) length)
              (find-widget-name-longer-than--low (rest low) length))]))                                    


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

