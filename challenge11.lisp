(in-package :protohackers)

(define-enum msg-type 1 ()
             (hello              #x50)
             (error-msg          #x51)
             (ok                 #x52)
             (dial-authority     #x53)
             (target-populations #x54)
             (create-policy      #x55)
             (delete-policy      #x56)
             (policy-result      #x57)
             (site-visit         #x58))

(defbinary hello (:byte-order :big-endian)
           (protocol "pestcontrol" :type (counted-string 4))
           (version 1              :type (unsigned-byte 32)))

(defbinary error-msg (:byte-order :big-endian)
           (message "bad" :type (counted-string 4)))

(defbinary ok ())

(defbinary dial-authority (:byte-order :big-endian)
           (site 0 :type (unsigned-byte 32)))

(defbinary target-entry (:byte-order :big-endian)
           (species "" :type (counted-string 4))
           (min 0      :type (unsigned-byte 32))
           (max 0      :type (unsigned-byte 32)))

(defbinary target-populations (:byte-order :big-endian)
           (site 0          :type (unsigned-byte 32))
           (populations nil :type (counted-array 4 target-entry)))

(define-enum action 1 ()
             (cull     #x90)
             (conserve #xa0))

(defbinary create-policy (:byte-order :big-endian)
           (species "" :type (counted-string 4))
           (action 0   :type action))

(defbinary delete-policy (:byte-order :big-endian)
           (policy 0 :type (unsigned-byte 32)))

(defbinary policy-result (:byte-order :big-endian)
           (policy 0 :type (unsigned-byte 32)))

(defbinary visit-entry (:byte-order :big-endian)
           (species "" :type (counted-string 4))
           (count 0    :type (unsigned-byte 32)))

(defbinary site-visit (:byte-order :big-endian)
           (site 0          :type (unsigned-byte 32))
           (populations nil :type (counted-array 4 visit-entry)))

(defbinary msg (:byte-order :big-endian)
           (type nil   :type msg-type)
           (length 0 :type (unsigned-byte 32))
           (data nil   :type (eval (identity type)))
           (checksum 0 :type (unsigned-byte 8)))

(defun initialize-msg (msg)
  (with-output-to-sequence (out)
    (setf (msg-length msg) (+ 6 (write-binary (msg-data msg) out))))
  (setf (msg-checksum msg) 0)
  (setf (msg-checksum msg)
        (mod (- 256 (reduce #'+ (with-output-to-sequence (out)
                                  (write-binary msg out))))
             256))
  msg)

(defun verify-msg (msg)
  (let-match1 (msg length checksum) msg
    (initialize-msg msg)
    (if (and (= length (msg-length msg))
             (= checksum (msg-checksum msg)))
        msg
        (error "invalid msg"))))

(setf (symbol-function 'write-msg) (write-binary-msg (compose #'initialize-msg #'make-msg)))

(defun read-msg (stream &optional eof-error-p eof-value)
  "EOF-ERROR-P and EOF-VALUE arguments are needed as we use this function
with iter as in-stream reader, but we ignore as lisp-binary throws EOF when
it get's an incomplete message it can't parse and we actually want to throw
an error in that case"
  (declare (ignore eof-error-p eof-value))
  (let* ((type (read-byte stream))
         (length (read-integer 4 stream :byte-order :big-endian))
         (data (make-array (- length 5))))
    (read-sequence data stream)
    (with-input-from-sequence (in (with-output-to-sequence (out)
                                    (write-byte type out)
                                    (write-integer length 4 out :byte-order :big-endian)
                                    (write-sequence data out)))
      (msg-data (verify-msg (read-binary 'msg in))))))

(defun handle-hello (&optional (stream *stream*))
  (write-msg (make-hello) stream)
  (unless (loop for byte across (with-output-to-sequence (out)
                                  (write-msg (make-hello) out))
                always (= byte (read-byte stream)))
    (error "bad hello")))

(defparameter *authorities* (synchronized-dict))

(defun decide-policy (populations observation)
  (with-slots (species count) observation
    (when-match (target-entry min max)
        (find species populations :test 'equal :key #'target-entry-species)
      (cond ((< count min) (make-create-policy :species species :action 'conserve))
            ((> count max) (make-create-policy :species species :action 'cull))))))

(defun authority (site)
  (with-client-socket (socket authority "pestcontrol.protohackers.com" 20547
                              :element-type '(unsigned-byte 8))
    (handle-hello authority)

    (write-msg (make-dial-authority :site site) authority)
    (with-slots (populations) (read-msg authority)
      (loop for visit = (pop-queue (@ *authorities* site))
            with defaults = (default-entries populations)
            with policies = (dict)
            do (loop for observation in (concatenate 'list defaults visit)
                     for policy = (decide-policy populations observation)
                     for species = (visit-entry-species observation)
                     for (old-policy . id) = (@ policies species)
                     do (when id
                          (write-msg (make-delete-policy :policy id) authority)
                          (assert (equalp (read-msg authority) (make-ok))))
                        (if policy
                            (progn (write-msg policy authority)
                                   (setf (@ policies species) (cons policy (policy-result-policy (read-msg authority)))))
                            (remhash species policies)))))))

(defun default-entries (populations)
  (loop for entry across populations
        collect (make-visit-entry :species (target-entry-species entry))))

(defun valid-site-visit? (msg)
  (with-slots (populations) msg
    (loop for entry across populations
          always (loop for other across populations
                       always (or (string/= (visit-entry-species entry) (visit-entry-species other))
                                  (= (visit-entry-count entry) (visit-entry-count other)))))))

(defmethod handle-req ((msg site-visit))
  (if (valid-site-visit? msg)
      (with-slots (site populations) msg
        (unless (@ *authorities* site)
          (setf (@ *authorities* site) (make-queue))
          (spawn (fmt "authority (site ~a)" site) (authority site)))
        (push-queue populations (@ *authorities* site)))
      (error "invalid msg")))

(serve 'handle-req
       :reader 'read-msg
       :writer 'write-msg
       :binary t
       :on-connect 'handle-hello
       :error-msg (make-error-msg))
