#lang racket
;;;; 2001: Presentation Timer
;;;;  - 2017 Risa YASAKA and Kazuhiro HISHINUMA.
(require racket/gui/base)


;;;
;;; Miscellaneous functions
;;;
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
;;; SCSI Timer
;;;
(define scsi-timer%
  (class object%
    (super-new)

    (field (time1 (* 8 60))
           (time2 (* 2 60))
           (time3 (* 3 60))
           (origins '(3)))
    (define elapsed 0)
    (define started #f)

    (define/public (is-started?)
      (if started #t #f))
    
    (define/public (get-elapsed (chap 0))
      (let ((value (if (is-started?)
                       (+ elapsed (- (current-seconds) started))
                       elapsed)))
        (cond ((= chap 1)
               (clamp 0 time1 value))
              ((= chap 2)
               (clamp 0 time2 (- value time1)))
              ((= chap 3)
               (clamp 0 time3 (- value time1 time2)))
              (else value))))

    (define/public (get-remaining (chap 0))
      (cond ((= chap 1)
             (- time1 (get-elapsed 1)))
            ((= chap 2)
             (- time2 (get-elapsed 2)))
            ((= chap 3)
             (- time3 (get-elapsed 3)))
            (else
             (let ((raw (get-elapsed)))
               (cond ((and (member 2 origins =) (< raw time1))
                      (- time1 raw))
                     ((and (member 3 origins =) (< raw (+ time1 time2)))
                      (- (+ time1 time2) raw))
                     (else
                      (max 0 (- (+ time1 time2 time3) raw))))))))

    (define/public (get-bell-status chap)
      (cond ((= chap 1)
             (<= time1 (get-elapsed)))
            ((= chap 2)
             (<= (+ time1 time2) (get-elapsed)))
            ((= chap 3)
             (<= (+ time1 time2 time3) (get-elapsed)))))
    
    (define/public (start!)
      (when (not (is-started?))
        (set! started (current-seconds))))
    
    (define/public (stop!)
      (set! elapsed (get-elapsed))
      (set! started #f))

    (define/public (reset!)
      (set! elapsed 0)
      (set! started #f))))
    
(define *scsi* (new scsi-timer%))


;;;
;;; 共通GUIパーツ
;;;
(define timer-view%
  (class canvas%
    (super-new)
    (init scsi)
    (define _scsi scsi)
    
    (new timer%
         (notify-callback (lambda()
                            (send this refresh)))
         (interval 20))
    
    (define/override (on-paint)
      (define-values (width height) (send this get-size))
      (define g (send this get-dc))
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
      (draw-button "BELL1" (* width 0.1) (* height 0.9) (* width 0.2) (* height 0.1)
                   (send _scsi get-bell-status 1))
      (draw-button "BELL2" (* width 0.4) (* height 0.9) (* width 0.2) (* height 0.1)
                   (send _scsi get-bell-status 2))
      (draw-button "BELL3" (* width 0.7) (* height 0.9) (* width 0.2) (* height 0.1)
                   (send _scsi get-bell-status 3))
      (void))))


;;;
;;; 表示ウィンドウ
;;;
(define view-frame%
  (class frame%
    (init scsi (width 640) (height 480))
    (super-new
     (label "View")
     (width width) (height height))
  
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

    (define tuner%
      (class group-box-panel%
        (init scsi bind-id)
        (define _scsi scsi)
        (define id bind-id)
        (define tid (string->symbol (format "time~A" bind-id)))
        (super-new
         (label (format "BELL~A" bind-id))
         (stretchable-height #f))
    
        (define hpane
          (new horizontal-panel% (parent this)
               (alignment '(left top))))
        (define field
          (new text-field% (parent hpane)
               (label #f)
               (init-value (number->string (quotient (dynamic-get-field tid _scsi) 60)))
               (callback (lambda (sender e)
                           (let ((value (string->number (send sender get-value))))
                             (when (natural? value)
                               (dynamic-set-field! tid _scsi (* value 60))))))))
        (new message% (parent hpane)
             (label "分"))
        (when (<= 2 id)
          (new check-box% (parent hpane)
               (label "残時間基準")
               (value (member id (get-field origins _scsi) =))
               (callback (lambda (sender e)
                           (if (send sender get-value)
                               (unless (member id (get-field origins _scsi) =)
                                 (set-field! origins _scsi (cons id (get-field origins _scsi))))
                               (set-field! origins _scsi (remove id (get-field origins _scsi) =)))))))
    
        (define status
          (new gauge% (parent this)
               (label (format-seconds (send _scsi get-remaining id)))
               (range (dynamic-get-field tid _scsi))))
        (new timer%
             (notify-callback (lambda ()
                                (cond ((zero? (dynamic-get-field tid _scsi))
                                       (send status set-label (format-seconds 0))
                                       (send status set-range id)
                                       (send status set-value (if (send _scsi get-bell-status id) 1 0)))
                                      (else
                                       (send status set-label (format-seconds (send _scsi get-remaining id)))
                                       (send status set-range (dynamic-get-field tid _scsi))
                                       (send status set-value (send _scsi get-elapsed id))))))
             (interval 20))))

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
                                              (send b set-label "停止"))))))))
      (new button% (parent top-panel)
           (label "初期化")
           (callback (lambda (b e)
                       (send scsi reset!)
                       (send start-btn set-label "開始")))))

    (new timer-view% (parent this)
         (style '(border))
         (min-width 320) (min-height 240)
         (scsi scsi))
    
    (new tuner% (parent this) (scsi scsi) (bind-id 1))
    (new tuner% (parent this) (scsi scsi) (bind-id 2))
    (new tuner% (parent this) (scsi scsi) (bind-id 3))))


;;;
;;; アプリケーションの起動
;;;
(send (new view-frame% (scsi *scsi*)) show #t)
(send (new controller-frame% (scsi *scsi*)) show #t)
