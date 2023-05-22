(defpackage :protohackers
  (:use cl iterate usocket lisp-binary)
  (:import-from :trivia when-match if-match plist let-match1 guard match)
  (:import-from :trivia.ppcre ppcre)
  (:import-from :serapeum @ dict fmt string-join receive vect ensure
                synchronized do-hash-table batches)
  (:import-from :flexi-streams
                string-to-octets octets-to-string make-in-memory-output-stream
                with-output-to-sequence with-input-from-sequence)
  (:import-from :lparallel.queue make-queue pop-queue push-queue)
  (:import-from :alexandria
                lastcar compose maxf extremum maphash-values if-let
                ensure-gethash when-let deletef emptyp hash-table-alist
                mean hash-table-keys hash-table-values last-elt)
  (:import-from :arrows -<>>)
  (:import-from :metabang-bind bind)
  (:import-from :trivial-do doalist)
  (:import-from :sb-ext defglobal atomic-incf with-locked-hash-table))
(in-package :protohackers)

(defvar *state*)
(defvar *stream*)

(defmacro spawn (&body body)
  (if (stringp (car body))
      `(bt:make-thread (lambda () ,@(cdr body)) :name ,(car body))
      `(bt:make-thread (lambda () ,@body))))

(defun serve (handler &key reader (writer #'do-nothing) binary
                        error-msg (error-writer writer) error-on-nil
                        (close-on-error t) on-connect on-close log)
  (socket-server "0.0.0.0" 47952
                 (lambda (stream)
                   (when log
                     (setf stream (make-logged-stream stream)))
                   (let ((*stream* stream)
                         *state*)
                     (handler-case
                         (progn
                           (when on-connect (funcall on-connect))
                           (iter (for input in-stream stream using reader)
                             (for (values response err) = (ignore-errors (funcall handler input)))
                             (when (and (null response) (or error-on-nil err))
                               (println "got error: ~a, on input: ~a" err input)
                               (write-flush error-msg :writer error-writer)
                               (when close-on-error
                                 (return)))
                             (write-flush response :writer writer))
                           (when on-close
                             (funcall on-close)))
                       (error (e)
                         (println "got error: ~a" e)
                         (write-flush error-msg :writer error-writer)))))
                 nil
                 :multi-threading t :in-new-thread t
                 :element-type (if binary
                                   '(unsigned-byte 8)
                                   'character)))

(defparameter *streams* nil)
(defun make-logged-stream (stream)
  (let ((input-copy (make-in-memory-output-stream))
        (output-copy (make-in-memory-output-stream)))
    (synchronized (*streams*)
      (setf *streams* (acons input-copy output-copy *streams*)))
    (make-two-way-stream (make-echo-stream stream input-copy)
                         (make-broadcast-stream stream output-copy))))

(defun read-binary-msg (msg-type data-fn)
  "binary reader for lisp-binary messages compatible with iterate"
  (lambda (stream eof-error-p eof-value)
    (handler-case
        (funcall data-fn (read-binary msg-type stream))
      (end-of-file ()
        (if eof-error-p
            (error 'end-of-file)
            eof-value)))))

(defun write-binary-msg (initializer)
  (lambda (obj stream)
    (prog1 (write-binary (funcall initializer :type (class-name (class-of obj)) :data obj) stream)
      (force-output stream))))

(defun do-nothing (&rest args)
  (declare (ignore args)))

(defun write-flush (msg &key (writer #'write-line) (stream *stream*))
  (ignore-errors
   (when msg
     (funcall writer msg stream)
     (force-output stream))))

(defun string-handler (f)
  (lambda (msg)
    (string-to-octets (funcall f (octets-to-string msg)))))

(defun udp-serve (handler)
  (socket-server "0.0.0.0" 47952 (string-handler handler) nil :protocol :datagram))

(defun read-json (stream eof-error-p eof-value)
  (let ((line (read-line stream eof-error-p eof-value)))
    (if (or (null line) (eql line eof-value))
        eof-value
        (ignore-errors (shasht:read-json line)))))

(defun write-json (json stream)
  (shasht:write-json* json :stream stream :pretty nil)
  (terpri stream))

(defparameter *stdout-lock* (bt:make-lock))
(defun println (&rest args)
  (bt:with-lock-held (*stdout-lock*)
    (apply #'format t args)
    (terpri)))

(defun synchronized-dict ()
  (make-hash-table :test 'equal :synchronized t))
