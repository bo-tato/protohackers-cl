(in-package :protohackers)

(defvar *db* (dict "version" "Ken's Key-Value Store 1.0"))

(defun handler (msg)
  (destructuring-bind (key &optional value)
      (str:split "=" msg :limit 2)
    (if value
        (unless (string= key "version")
          (setf (@ *db* key) value)
          nil)
        (fmt "~a=~a" key (@ *db* key)))))

(udp-serve 'handler)
