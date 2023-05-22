(in-package :protohackers)

(defstruct session received ackd sent-data input)
(defparameter *sessions* (dict))
(defvar +data-pattern+ (ppcre:create-scanner "^/data/(\\d+)/(\\d+)/(.*)/$" :single-line-mode t))

(defun unescape (data)
  (str:replace-using '("\\\\" "\\"
                       "\\/" "/")
                     data))

(defun escape (data)
  (str:replace-using '("\\" "\\\\"
                       "/" "\\/")
                     data))

(defun close-session (session)
  (remhash session *sessions*)
  (fmt "/close/~a/" session))

(defun data-msg (session pos data)
  (fmt "/data/~a/~a/~a/" session pos (escape data)))

(defun send-data (session data)
  (dolist (data (batches (fmt "~a~%" (reverse data)) 900))
    (bind (((:slots ackd sent-data) (@ *sessions* session))
           (msg (data-msg session (length sent-data) data))
           (need-ack (length (setf sent-data (str:concat sent-data data))))
           (socket usocket::*server*)
           (host *remote-host*)
           (port *remote-port*))
      (spawn "sender"
        (loop repeat 20
              when (>= ackd need-ack) return nil
                do (socket-send socket msg nil :host host :port port)
                   (sleep 3)
              finally (close-session session))))))

(defun recv-data (session data)
  (bind (((:slots input) (@ *sessions* session))
         (lines (str:split #\Newline (str:concat input data))))
    (dolist (line (butlast lines))
      (send-data session line))
    (setf input (lastcar lines))))

(defun handler (msg)
  (match msg
    ((ppcre "^/connect/(\\d+)/$" session)
     (ensure-gethash session *sessions* (make-session :received 0 :ackd 0 :sent-data "" :input ""))
     (fmt "/ack/~a/0/" session))
    ((ppcre +data-pattern+ session pos data)
     (unless (ppcre:scan "[^\\\\]/" data) ; check for bad unescaped data
       (if (@ *sessions* session)
           (with-slots (received input) (@ *sessions* session)
             (if (= (parse-integer pos) received)
                 (let ((data (unescape data)))
                   (recv-data session data)
                   (fmt "/ack/~a/~a/" session (incf received (length data))))
                 (fmt "/ack/~a/~a/" session received)))
           (close-session session))))
    ((ppcre "^/ack/(\\d+)/(\\d+)/$" session length)
     (if (@ *sessions* session)
         (with-slots (ackd sent-data) (@ *sessions* session)
           (let ((sent (length sent-data))
                 (length (parse-integer length)))
             (maxf ackd length)
             (cond ((< length ackd) nil)
                   ((> length sent) (close-session session))
                   ((< length sent) (data-msg session length (subseq sent-data length))))))
         (close-session session)))
    ((ppcre "^/close/(\\d+)/$" session)
     (close-session session))))

(udp-serve 'handler)
