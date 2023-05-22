(in-package :protohackers)

(defstruct direntry dir file)
(defparameter *fs* (synchronized-dict))
(defvar +help+
"OK usage: HELP|GET|PUT|LIST
READY")

(defun create-directories (path)
  (reduce (lambda (cwd dir)
            (ensure (direntry-dir
                     (ensure (@ cwd dir)
                       (make-direntry :dir (synchronized-dict))))
              (synchronized-dict)))
          path
          :initial-value *fs*))

(defun split-path (path)
  (str:split "/" path))

(defun get-file (name)
  (let ((path (split-path name)))
    (direntry-file (ensure (@ (create-directories (butlast path)) (lastcar path))
                     (make-direntry :file (vect))))))

(defun put-file (file contents)
  (if (and (not (emptyp file)) (equal (last-elt file) contents))
      (length file)
      (1+ (vector-push-extend contents file))))

(defun printable-ascii? (string)
  (loop for c across string
        always (case c
                 ((#\Newline #\Tab) t)
                 (t (<= 32 (char-code c) 127)))))

(defun handler (line)
  (match line
    ((ppcre "^(?i)PUT ([\\w-./]*[\\w-.]) (\\d+)$" name size)
     (let* ((size (parse-integer size))
            (contents (make-string size)))
       (read-sequence contents *stream*)
       (when (and (plusp size) (printable-ascii? contents))
         (fmt "OK r~a~%READY" (put-file (get-file name) contents)))))
    ((ppcre "^(?i)GET ([\\w-./]*)( r)?(\\d+)?$" name _ revision)
     (let* ((file (get-file name))
            (revision (1- (if revision
                              (parse-integer revision)
                              (length file))))
            (contents (elt file revision)))
       (fmt "OK ~a~%~aREADY" (length contents) contents)))
    ((ppcre "^(?i)LIST (/[\\w-./]*)$" path)
     (let ((dir (create-directories
                 (split-path (str:trim-right path :char-bag "/")))))
       (write-flush (fmt "OK ~a" (hash-table-count dir)))
       (doalist (name entry (sort (hash-table-alist dir) #'string< :key #'car))
         (when (direntry-dir entry)
           (write-flush (fmt "~a/ DIR" name)))
         (when-let (file (direntry-file entry))
           (write-flush (fmt "~a r~a" name (length file)))))
       "READY"))
    ((ppcre "^(?i)HELP") +help+)))

(serve 'handler :reader #'read-line
                :writer #'write-line
                :error-msg "ERR illegal command"
                :error-on-nil t
                :on-connect (lambda () (write-flush "READY")))
