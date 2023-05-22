(in-package :protohackers)

(defglobal *id* 1)
(declaim (type fixnum *id*))

(defstruct job pri id queue job)
(defparameter *jobs* (synchronized-dict))

(defun get-jobs (queues)
  (loop for queue across queues
        appending (@ *jobs* queue)))

(defun has-id? (id)
  (lambda (job)
    (= (job-id job) id)))

(defun find-job (id)
  (loop for queue in (hash-table-values *jobs*)
        thereis (find-if (has-id? id) queue)))

(defun delete-job (job)
  (dolist (queue (hash-table-keys *jobs*))
      (deletef (@ *jobs* queue) job)))

(defun delete-id (id)
  (with-locked-hash-table (*jobs*)
    (when-let (job (find-job id))
      (delete-job job)
      t)))

(defun pop-job (queues)
  "returns the job with max priority out of queues, or nil if all empty"
  (with-locked-hash-table (*jobs*)
    (when-let (job (extremum (get-jobs queues) #'> :key #'job-pri))
      (delete-job job)
      (push job (@ *jobs* (bt:current-thread)))
      job)))

(defun add-job (job)
  (push job (@ *jobs* (job-queue job))))

(defun handler (req)
  (match req
    ;; PUT
    ((dict "request" (guard s (string= s "put"))
           "queue" queue
           "job" job
           "pri" (guard pri (and (integerp pri) (not (minusp pri)))))
     (let ((id (atomic-incf *id*)))
       (with-locked-hash-table (*jobs*)
         (add-job (make-job :id id :pri pri :job job :queue queue)))
       (dict "status" "ok"
             "id" id)))
    ;; GET
    ((dict "request" (guard s (string= s "get"))
           "queues" queues
           "wait" wait)
     (if-let (job (pop-job queues))
       (dict "status" "ok"
             "id" (job-id job)
             "job" (job-job job)
             "pri" (job-pri job)
             "queue" (job-queue job))
       (if wait
           (progn (sleep 1) (handler req))
           (dict "status" "no-job"))))
    ;; DELETE
    ((dict "request" (guard s (string= s "delete"))
           "id" id)
     (if (delete-id id)
         (dict "status" "ok")
         (dict "status" "no-job")))
    ;; ABORT
    ((dict "request" (guard s (string= s "abort"))
           "id" id)
     (with-locked-hash-table (*jobs*)
       (if-let (job (find-if (has-id? id) (@ *jobs* (bt:current-thread))))
         (progn
           (deletef (@ *jobs* (bt:current-thread)) job)
           (add-job job)
           (dict "status" "ok"))
         (dict "status" "no-job"))))))

(defun abort-jobs ()
  (with-locked-hash-table (*jobs*)
    (mapc #'add-job (@ *jobs* (bt:current-thread)))
    (remhash (bt:current-thread) *jobs*)))

(serve 'handler :reader #'read-json
                :writer #'write-json
                :error-msg (dict "status" "error")
                :error-on-nil t
                :close-on-error nil
                :on-close 'abort-jobs)
