#lang racket
;;;; 2001: Presentation Timer
;;;;  - 2017 Risa YASAKA and Kazuhiro HISHINUMA.
(require racket/gui/base)


;;;
;;;
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
;;;
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
      (set! started #f))
    
    (define/public (draw g width height)
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
      (draw-text/size (format-seconds (get-remaining))
                      0 (* height 0.2) width (* height 0.6) 'center)
      (draw-button "BELL1" (* width 0.1) (* height 0.9) (* width 0.2) (* height 0.1)
                   (get-bell-status 1))
      (draw-button "BELL2" (* width 0.4) (* height 0.9) (* width 0.2) (* height 0.1)
                   (get-bell-status 2))
      (draw-button "BELL3" (* width 0.7) (* height 0.9) (* width 0.2) (* height 0.1)
                   (get-bell-status 3))
      (void))
    ))

(define *scsi* (new scsi-timer%))


;;;
;;;
;;;
(define *view-frame*
  (new frame% (label "Presentation Timer")
       (width 640) (height 480)))

(let ((view (new canvas% (parent *view-frame*)
                   (paint-callback (lambda (c g)
                                     (let-values (((width height) (send c get-size))) 
                                       (send *scsi* draw g width height)))))))
  (new timer%
       (notify-callback (lambda () (send view refresh)))
       (interval 20)))

(send *view-frame* show #t)


;;;
;;;
;;;
(define *controller-frame*
  (new frame% (label "Controller")))

(let* ((top-panel (new horizontal-panel% (parent *controller-frame*)
                       (alignment '(left top)) (stretchable-height #f)))
       (start-btn (new button% (parent top-panel)
                       (label "開始")
                       (callback (lambda (b e)
                                   (cond ((string=? (send b get-label) "停止")
                                          (send *scsi* stop!)
                                          (send b set-label "開始"))
                                         (else
                                          (send *scsi* start!)
                                          (send b set-label "停止"))))))))
  (new button% (parent top-panel)
       (label "初期化")
       (callback (lambda (b e)
                   (send *scsi* reset!)
                   (send start-btn set-label "開始")))))

(let ((monitor (new canvas% (parent *controller-frame*)
                    (style '(border))
                    (min-width 320) (min-height 240)
                    (paint-callback (lambda (c g)
                                      (let-values (((width height) (send c get-size)))
                                        (send *scsi* draw g width height)))))))
  (new timer%
       (notify-callback (lambda () (send monitor refresh)))
       (interval 20)))

(let* ((group (new group-box-panel% (parent *controller-frame*)
                   (label "BELL1") (stretchable-height #f)))
       (hpane (new horizontal-panel% (parent group)
                   (alignment '(left top))))
       (field (new text-field% (parent hpane)
                   (label #f)
                   (init-value (number->string (quotient (get-field time1 *scsi*) 60)))
               (callback (lambda (sender e)
                           (let ((value (string->number (send sender get-value))))
                             (when (natural? value)
                               (set-field! time1 *scsi* (* value 60))))))))
       (_ (new message% (parent hpane) (label "分")))
       (status (new gauge% (parent group)
                    (label (format-seconds (send *scsi* get-remaining 1)))
                    (range (get-field time1 *scsi*)))))
  (new timer%
       (notify-callback (lambda ()
                          (cond ((zero? (get-field time1 *scsi*))
                                 (send status set-label (format-seconds 0))
                                 (send status set-range 1)
                                 (send status set-value (if (send *scsi* get-bell-status 1) 1 0)))
                                (else
                                 (send status set-label (format-seconds (send *scsi* get-remaining 1)))
                                 (send status set-range (get-field time1 *scsi*))
                                 (send status set-value (send *scsi* get-elapsed 1))))))
       (interval 20)))

(let* ((group (new group-box-panel% (parent *controller-frame*)
                   (label "BELL2") (stretchable-height #f)))
       (hpane (new horizontal-panel% (parent group)
                   (alignment '(left top))))
       (field (new text-field% (parent hpane)
                   (label #f)
                   (init-value (number->string (quotient (get-field time2 *scsi*) 60)))
               (callback (lambda (sender e)
                           (let ((value (string->number (send sender get-value))))
                             (when (natural? value)
                               (set-field! time2 *scsi* (* value 60))))))))
       (_ (new message% (parent hpane) (label "分")))
       (_ (new check-box% (parent hpane)
               (label "残時間基準")
               (value (member 2 (get-field origins *scsi*) =))
               (callback (lambda (sender e)
                           (if (send sender get-value)
                               (unless (member 2 (get-field origins *scsi*) =)
                                 (set-field! origins *scsi* (cons 2 (get-field origins *scsi*))))
                               (set-field! origins *scsi* (remove 2 (get-field origins *scsi*) =)))))))
       (status (new gauge% (parent group)
                    (label (format-seconds (send *scsi* get-remaining 1)))
                    (range (get-field time2 *scsi*)))))
  (new timer%
       (notify-callback (lambda ()
                          (cond ((zero? (get-field time2 *scsi*))
                                 (send status set-label (format-seconds 0))
                                 (send status set-range 1)
                                 (send status set-value (if (send *scsi* get-bell-status 2) 1 0)))
                                (else
                                 (send status set-label (format-seconds (send *scsi* get-remaining 2)))
                                 (send status set-range (get-field time2 *scsi*))
                                 (send status set-value (send *scsi* get-elapsed 2))))))
       (interval 20)))

(let* ((group (new group-box-panel% (parent *controller-frame*)
                   (label "BELL3") (stretchable-height #f)))
       (hpane (new horizontal-panel% (parent group)
                   (alignment '(left top))))
       (field (new text-field% (parent hpane)
                   (label #f)
                   (init-value (number->string (quotient (get-field time3 *scsi*) 60)))
               (callback (lambda (sender e)
                           (let ((value (string->number (send sender get-value))))
                             (when (natural? value)
                               (set-field! time3 *scsi* (* value 60))))))))
       (_ (new message% (parent hpane) (label "分")))
       (_ (new check-box% (parent hpane)
               (label "残時間基準")
               (value (member 3 (get-field origins *scsi*) =))
               (callback (lambda (sender e)
                           (if (send sender get-value)
                               (unless (member 3 (get-field origins *scsi*) =)
                                 (set-field! origins *scsi* (cons 3 (get-field origins *scsi*))))
                               (set-field! origins *scsi* (remove 3 (get-field origins *scsi*) =)))))))
       (status (new gauge% (parent group)
                    (label (format-seconds (send *scsi* get-remaining 1)))
                    (range (get-field time3 *scsi*)))))
  (new timer%
       (notify-callback (lambda ()
                          (cond ((zero? (get-field time3 *scsi*))
                                 (send status set-label (format-seconds 0))
                                 (send status set-range 1)
                                 (send status set-value (if (send *scsi* get-bell-status 3) 1 0)))
                                (else
                                 (send status set-label (format-seconds (send *scsi* get-remaining 3)))
                                 (send status set-range (get-field time3 *scsi*))
                                 (send status set-value (send *scsi* get-elapsed 3))))))
       (interval 20)))

(send *controller-frame* show #t)
