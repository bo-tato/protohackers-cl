(in-package :protohackers)

(defparameter *clients* (synchronized-dict))

(defun valid-name? (name)
  (ppcre:scan "^[a-zA-Z0-9]+$" name))

(defun send-rest (name message)
  (do-hash-table (user *stream* *clients*)
    (when (string/= name user)
      (write-flush message))))

;; *state* will store the name for the client on the connection
(defun new-client ()
  (write-flush "Welcome to budgetchat! What shall I call you?")
  (let ((name (read-line *stream*)))
    (unless (valid-name? name) (error "invalid name"))
    (write-flush (fmt "* The room contains: ~a" (string-join (hash-table-keys *clients*) ", ")))
    (send-rest name (fmt "* ~a has entered the room" name))
    (setf *state* name)
    (setf (@ *clients* name) *stream*)))

(defun handle-message (msg)
  (send-rest *state* (fmt "[~a] ~a" *state* msg)))

(defun leave-room ()
  (remhash *state* *clients*)
  (send-rest *state* (fmt "* ~a has left the room" *state*)))

(serve 'handle-message :reader #'read-line
                       :on-connect 'new-client
                       :on-close 'leave-room)
