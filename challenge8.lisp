(in-package :protohackers)

(defvar *received*)
(defvar *sent*)

(defun reverse-bits (n)
  (let ((result 0))
    (dotimes (i 8 result)
      (setf result (ash result 1))
      (when (logbitp i n)
        (incf result)))))

(defun xor-by (n)
  (lambda (x)
    (logxor x n)))

(defun xor-received (n)
  (logxor n (mod *received* 256)))

(defun xor-sent (n)
  (logxor n (mod *sent* 256)))

(defun add-n (n)
  (lambda (x)
    (mod (+ x n) 256)))

(defun add-sent (n)
  (mod (+ n *sent*) 256))

(defun sub-received (n)
  (mod (- n *received*) 256))

(defun get-cipher (stream)
  (iter (with encrypt = (list #'identity))
    (with decrypt = (list #'identity))
    (for byte in-stream stream using #'read-byte)
    (until (zerop byte))
    (ecase byte
      (1 (push #'reverse-bits decrypt)
       (push #'reverse-bits encrypt))
      (2 (let ((f (xor-by (read-byte stream))))
           (push f decrypt)
           (push f encrypt)))
      (3 (push #'xor-received decrypt)
       (push #'xor-sent encrypt))
      (4 (let ((n (read-byte stream)))
           (push (add-n n) encrypt)
           (push (add-n (- 256 n)) decrypt)))
      (5 (push #'add-sent encrypt)
       (push #'sub-received decrypt)))
    (finally (return (values (apply #'compose encrypt)
                             (apply #'compose (nreverse decrypt)))))))

(defun most-copies (line)
  (extremum (str:split "," line) #'>
            :key (lambda (s) (parse-integer s :junk-allowed t))))

(defun valid-cipher? (cipher)
  (let ((*received* 1))
    (/= 1 (funcall cipher 1))))

(defun handler (stream)
  (receive (encrypt decrypt) (get-cipher stream)
    (when (valid-cipher? decrypt)
      (iter (with line = '())
        (with *sent* = 0)
        (for byte in-stream stream using #'read-byte)
        (for *received* from 0)
        (for decoded = (code-char (funcall decrypt byte)))
        (if (char= decoded #\Newline)
            (progn (-<>> (nreverse line)
                     (coerce <> 'string)
                     most-copies
                     (fmt "~a~%")
                     (loop for c across <>
                           collect (funcall encrypt (char-code c))
                           do (incf *sent*))
                     (write-flush <> :writer #'write-sequence :stream stream))
                   (setq line '()))
            (push decoded line))))))

(socket-server "0.0.0.0" 47952 'handler nil
               :multi-threading t :in-new-thread t :element-type '(unsigned-byte 8))
