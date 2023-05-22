(in-package :protohackers)

(define-enum msg-type 1 ()
             (error-msg       #x10)
             (ticket          #x21)
             (heartbeat       #x41)
             (plate           #x20)
             (want-heartbeat  #x40)
             (am-camera       #x80)
             (am-dispatcher   #x81))

(defbinary plate (:byte-order :big-endian)
           (plate ""     :type (counted-string 1 :external-format :utf8))
           (timestamp 0  :type (unsigned-byte 32)))

(defbinary want-heartbeat (:byte-order :big-endian)
           (interval 0 :type (unsigned-byte 32)))

(defbinary am-camera (:byte-order :big-endian)
           (road 0   :type (unsigned-byte 16))
           (mile 0   :type (unsigned-byte 16))
           (limit 0  :type (unsigned-byte 16)))

(defbinary error-msg (:byte-order :big-endian)
           (msg "" :type (counted-string 1 :external-format :utf8)))

(defbinary ticket (:byte-order :big-endian)
           (plate ""     :type (counted-string 1 :external-format :utf8))
           (road 0       :type (unsigned-byte 16))
           (mile1 0      :type (unsigned-byte 16))
           (timestamp1 0 :type (unsigned-byte 32))
           (mile2 0      :type (unsigned-byte 16))
           (timestamp2 0 :type (unsigned-byte 32))
           (speed 0      :type (unsigned-byte 16)))

(defbinary am-dispatcher (:byte-order :big-endian)
           (roads #() :type (counted-array 1 (unsigned-byte 16))))

(defbinary heartbeat ())

(defbinary msg (:byte-order :big-endian)
           (type nil :type msg-type)
           (data nil :type (eval (identity type))))

(setf (symbol-function 'write-msg) (write-binary-msg #'make-msg))

(defmethod handle-req ((msg am-camera))
  (if *state*
      (error "already set")
      (setf *state* msg)))

(defparameter *dispatchers* (synchronized-dict))
(defparameter *pending-tickets* (synchronized-dict))
(defmethod handle-req ((msg am-dispatcher))
  (when *state* (error "already set"))
  (setf *state* msg)
  (loop for road across (slot-value msg 'roads)
        do (setf (@ *dispatchers* road) *stream*)
           (dolist (ticket (@ *pending-tickets* road))
             (write-msg ticket *stream*))))

(defparameter *readings* (make-queue))
(defmethod handle-req ((msg plate))
  (bind (((:slots road mile limit) *state*)
         ((:slots plate timestamp) msg))
    (push-queue (list road mile limit plate timestamp) *readings*)))

(defmethod handle-req ((msg want-heartbeat))
  (bind (((:slots interval) msg)
         (stream *stream*))
    (when (plusp interval)
      (spawn "heartbeat thread"
        (loop while (write-msg (make-heartbeat) stream)
              do (sleep (/ interval 10)))))))

(defun timestamp->day (timestamp)
  (floor (/ timestamp 86400)))

(defun no-day-overlap? (timestamp1 timestamp2 sent-tickets)
  (loop with start1 = (timestamp->day timestamp1)
        with end1 = (timestamp->day timestamp2)
        for (start2 . end2) in sent-tickets
        always (or (< end1 (timestamp->day start2))
                   (> start1 (timestamp->day end2)))))

(defun find-speeder (readings mile timestamp limit sent-tickets)
  (loop for (mile2 . timestamp2) in readings
        when (/= mile mile2)
          do (bind ((((timestamp1 . mile1) (timestamp2 . mile2))
                     (sort `((,timestamp . ,mile) (,timestamp2 . ,mile2))
                           (lambda (r1 r2) (< (car r1) (car r2)))))
                    (speed (* 3600 (/ (abs (- mile1 mile2))
                                      (- timestamp2 timestamp1)))))
               (when (and (> speed limit)
                          (no-day-overlap? timestamp1 timestamp2 sent-tickets))
                 (return (list mile1 timestamp1 mile2 timestamp2 speed))))))

;; thead that will process all plate readings and send tickets
(spawn "ticketer"
  (loop with plates-seen = (dict)
        with sent-tickets = (dict)
        for (road mile limit plate timestamp) = (pop-queue *readings*)
        for road-plate = (cons road plate)
        for readings-for-car = (push (cons mile timestamp)
                                     (@ plates-seen road-plate))
        for tickets-for-car = (@ sent-tickets road-plate)
        do (when-match (list mile1 timestamp1 mile2 timestamp2 speed)
               (find-speeder readings-for-car mile timestamp limit tickets-for-car)
             (push (cons timestamp1 timestamp2) (@ sent-tickets road-plate))
             (let ((ticket (make-ticket :plate plate :road road :mile1 mile1 :timestamp1 timestamp1
                                        :mile2 mile2 :timestamp2 timestamp2 :speed (round (* 100 speed)))))
               (if-let (dispatcher (@ *dispatchers* road))
                 (write-msg ticket dispatcher)
                 (push ticket (@ *pending-tickets* road)))))))

(serve 'handle-req
       :reader (read-binary-msg 'msg #'msg-data)
       :binary t
       :error-msg (make-error-msg :msg "illegal msg")
       :error-writer #'write-msg)
