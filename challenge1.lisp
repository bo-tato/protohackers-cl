(in-package :protohackers)

(defun primep (n)
  (cond
    ((not (integerp n)) nil)
    ((< n 2) nil)
    (t (loop for i from 2 to (sqrt n)
             never (zerop (mod n i))))))

(defun handle-req (req)
  (when-match
      (dict "method" (guard method (string= method "isPrime"))
            "number" (guard num (numberp num)))
      req
    (dict "method" "isPrime"
          "prime" (primep num))))

(serve 'handle-req
       :reader #'read-json
       :writer #'write-json
       :error-msg "malformed"
       :error-on-nil t)
