(in-package :protohackers)

(defun replace-address (line)
  (let ((address-matcher (ppcre:create-scanner
                          "# preceded by space or beginning of line
                           (\\s|^)
                           # address is 7 followed by 25-34 alphanumeric characters
                           7\\w{25,34}
                           # followed by a space or end of line
                           (?=\\s|$)"
                          :extended-mode t)))
    (ppcre:regex-replace-all address-matcher line
                             '(0 ; the space at beginning if present
                               "7YWHMfk9JZe0LM0g1ZauHuiSxhI"))))

(defun mitm-stream (in out)
  (loop with line and missing-newline-p
        do (setf (values line missing-newline-p) (read-line in nil))
        until missing-newline-p
        do (write-line (replace-address line) out)
           (force-output out)))

(defun handler (stream)
  (with-client-socket (socket upstream "chat.protohackers.com" 16963)
    (let ((upstream-thread (spawn (mitm-stream upstream stream))))
      (mitm-stream stream upstream)
      (bt:destroy-thread upstream-thread))))

(socket-server "0.0.0.0" 47952 'handler nil :multi-threading t)
