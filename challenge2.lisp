(in-package :protohackers)

(define-enum msg-type 1 ()
    (insert #x49)
    (query #x51))

(defbinary insert (:byte-order :big-endian)
           (timestamp 0 :type (signed-byte 32))
           (price 0 :type (signed-byte 32)))

(defbinary query (:byte-order :big-endian)
           (mintime 0 :type (signed-byte 32))
           (maxtime 0 :type (signed-byte 32)))

(defbinary msg (:byte-order :big-endian)
           (type nil :type msg-type)
           (data nil :type (eval (case type
                                   (insert 'insert)
                                   (query 'query)))))

(defbinary response (:byte-order :big-endian)
           (average 0 :type (signed-byte 32)))

(defbinary empty-response ())

(defmethod handle-req ((msg insert))
  (with-slots (timestamp price) msg
    (setf *state* (acons timestamp price *state*)))
  (make-empty-response))

(defmethod handle-req ((msg query))
  (with-slots (mintime maxtime) msg
    (make-response :average
                   (round (mean
                           (or
                            (loop for (timestamp . price) in *state*
                                  when (<= mintime timestamp maxtime)
                                    collect price)
                            '(0)))))))

(serve 'handle-req
       :reader (read-binary-msg 'msg #'msg-data)
       :writer #'write-binary
       :binary t)
