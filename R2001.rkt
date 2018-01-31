#lang racket
;;;; 2001: Presentation Timer
;;;;  - 2017 Risa YASAKA and Kazuhiro HISHINUMA.
(require racket/gui/base)


;;;
;;; Miscellaneous functions
;;;
(define (pa$ f . p-args)
  (lambda s-args
    (apply f (append p-args s-args))))

(define (clamp min max x)
  (cond ((<= x min) min)
        ((<= max x) max)
        (else x)))
  
(define (format-seconds s)
  (define (pad x n)
    (~a x #:min-width n #:align 'right #:pad-string "0"))
  (let-values (((m s) (quotient/remainder s 60)))
    (format "~A:~A" (pad m 2) (pad s 2))))


;;;
;;; SCSI
;;;
(define scsi%
  (class object%
    (super-new)

    (define elapsed 0)
    (define started #f)

    (define bells (map (pa$ * 60) '(4 5 8)))
    (define origins '(3))
    
    (define/public (get-bell chap)
      (apply max 0 (take bells chap)))
    
    (define/public (set-bell! chap value)
      (set! bells (let-values (((pre post) (split-at bells (- chap 1))))
                    (append pre (list value) (cdr post)))))

    (define/public (get-bell-length chap)
      (- (get-bell chap) (get-bell (- chap 1))))

    (define/public (is-origin? chap)
      (if (member chap origins =) #t #f))
    
    (define/public (set-origin chap (value #t))
      (if value
          (unless (is-origin? chap)
            (set! origins (cons chap origins)))
          (unset-origin chap)))

    (define/public (unset-origin chap)
      (set! origins (remove chap origins =)))
    
    (define/public (is-started?)
      (if started #t #f))

    (define/public (get-raw-elapsed)
      elapsed)
    
    (define/public (get-elapsed (chap 0))
      (if (= chap 0) elapsed
          (- (clamp (get-bell (- chap 1)) (get-bell chap) elapsed) (get-bell (- chap 1)))))

    (define/public (get-remaining (chap 0))
      (if (= chap 0)
          (let lp ((ls origins))
            (cond ((null? ls)
                   (max 0 (- (car (take-right bells 1)) elapsed)))
                  ((< elapsed (get-bell (- (car ls) 1)))
                   (- (get-bell (- (car ls) 1)) elapsed))
                  (else (lp (cdr ls)))))
          (- (get-bell chap) (clamp (get-bell (- chap 1)) (get-bell chap) elapsed))))

    (define/public (get-bell-status chap)
      (<= (get-bell chap) elapsed))

    (define/private (refresh!)
      (if (is-started?)
          (let* ((t (- (current-seconds) started))
                 (res (= elapsed t)))
            (set! elapsed t)
            res)
          #f))
    
    (define/public (start!)
      (when (not (is-started?))
        (set! started (- (current-seconds) elapsed))))
    
    (define/public (stop!)
      (when (is-started?)
        (refresh!)
        (set! started #f)))

    (define/public (reset!)
      (set! elapsed 0)
      (set! started #f))

    (define callbacks '())
    (define/public (add-callback! callback)
      (set! callbacks (cons callback callbacks)))
    
    (new timer% (interval 50)
         (notify-callback
          (lambda ()
            (refresh!)
            (for ((callback callbacks))
              (callback)))))))


;;;
;;; 共通GUIパーツ
;;;
(define timer-view%
  (class canvas%
    (super-new)
    (init scsi)
    (define _scsi scsi)
    (inherit get-size get-dc refresh)
    
    (define/override (on-paint)
      (define-values (width height) (get-size))
      (define g (get-dc))
      (define (draw-text/size text x y width height (align 'left))
        (let*-values (((w h _1 _2) (send g get-text-extent text))
                      ((scal) (min (/ width w) (/ height h)))
                      ((dx) (cond ((eq? align 'center) (/ (- width (* w scal)) 2))
                                  ((eq? align 'right) (- width (* w scal)))
                                  (else 0)))
                      ((dy) (/ (- height (* h scal)) 2)))
          (send g set-origin (+ x dx) (+ y dy))
          (send g set-scale scal scal)
          (send g draw-text text 0 0)
          (send g set-origin 0 0)
          (send g set-scale 1 1)))
      (define (draw-button text x y width height status)
        (cond (status
               (send g set-text-foreground "Black")
               (send g set-brush (make-brush #:color "Orange")))
              (else
               (send g set-text-foreground "Orange")
               (send g set-brush (make-brush #:style 'transparent))))
        (send g set-pen (make-pen #:color "Orange"))
        (send g draw-rectangle x y width height)
        (draw-text/size text x y width height 'center))
      
      (send g set-background "Black")
      (send g clear)
      (send g set-text-foreground "Orange")
      (draw-text/size "残り時間" 0 0 width (* height 0.2))
      (draw-text/size (format-seconds (send _scsi get-remaining))
                      0 (* height 0.2) width (* height 0.6) 'center)
      (for ((i '(1 2 3)))
        (draw-button (format "BELL~A" i)
                     (* width (+ 0.1 (* 0.3 (- i 1)))) (* height 0.9)
                     (* width 0.2) (* height 0.1)
                     (send _scsi get-bell-status i))))

    (send scsi add-callback!
          (lambda () (refresh)))))


;;;
;;; 表示ウィンドウ
;;;
(define view-frame%
  (class frame%
    (init scsi (width 640) (height 480))
    (super-new
     (label "View")
     (width width) (height height))

    (define/augment (can-close?) #f)
  
    (new timer-view% (parent this)
         (scsi scsi))))


;;;
;;; 制御ウィンドウ
;;;
(define controller-frame%
  (class frame%
    (super-new
     (label "Controller"))
    (init scsi)

    (define/augment (on-close)
      (exit))
    
    (define tuner%
      (class group-box-panel%
        (init scsi id)
        (super-new
         (label (format "BELL~A" id))
         (stretchable-height #f))
    
        (define hpane
          (new horizontal-panel% (parent this)
               (alignment '(left top))))
        (define field
          (new text-field% (parent hpane)
               (label #f)
               (init-value (number->string (quotient (send scsi get-bell id) 60)))
               (callback (lambda (sender e)
                           (let ((value (string->number (send sender get-value))))
                             (when (natural? value)
                               (send scsi set-bell! id (* value 60))))))))
        (new message% (parent hpane)
             (label "分"))
        (when (<= 2 id)
          (new check-box% (parent hpane)
               (label "残時間基準")
               (value (send scsi is-origin? id))
               (callback (lambda (sender e)
                           (if (send sender get-value)
                               (send scsi set-origin id)
                               (send scsi unset-origin id))))))
    
        (define status
          (new gauge% (parent this)
               (label (format-seconds (send scsi get-remaining id)))
               (range (send scsi get-bell-length id))))
        (send scsi add-callback!
              (lambda ()
                (cond ((zero? (send scsi get-bell-length id))
                       (send status set-label (format-seconds 0))
                       (send status set-range 1)
                       (send status set-value (if (send scsi get-bell-status id) 1 0)))
                      (else
                       (send status set-label (format-seconds (send scsi get-remaining id)))
                       (send status set-range (send scsi get-bell-length id))
                       (send status set-value (send scsi get-elapsed id))))))))

    (let* ((top-panel (new horizontal-panel% (parent this)
                           (alignment '(left top)) (stretchable-height #f)))
           (start-btn (new button% (parent top-panel)
                           (label "開始")
                           (callback (lambda (b e)
                                       (cond ((string=? (send b get-label) "停止")
                                              (send scsi stop!)
                                              (send b set-label "開始"))
                                             (else
                                              (send scsi start!)
                                              (send b set-label "停止")))))))
           (_ (new button% (parent top-panel)
                   (label "初期化")
                   (callback (lambda (b e)
                               (send scsi reset!)
                               (send start-btn set-label "開始")))))
           (elapsed (new message% (parent top-panel)
                         (label (string-append "経過時間: " (format-seconds 0))))))
      (send scsi add-callback!
            (lambda ()
              (send elapsed set-label (string-append "経過時間: " (format-seconds (send scsi get-raw-elapsed)))))))
           
                  

    
    (new timer-view% (parent this)
         (style '(border))
         (min-width 320) (min-height 240)
         (scsi scsi))

    (new tuner% (parent this) (scsi scsi) (id 1))
    (new tuner% (parent this) (scsi scsi) (id 2))
    (new tuner% (parent this) (scsi scsi) (id 3))))


;;;
;;; アプリケーションの起動
;;;
(define *scsi* (new scsi%))
(send (new view-frame% (scsi *scsi*)) show #t)
(send (new controller-frame% (scsi *scsi*)) show #t)
