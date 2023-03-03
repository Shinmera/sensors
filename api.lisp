(in-package #:sensors)

(defun ht (&rest kv)
  (let ((table (make-hash-table :test 'eq)))
    (loop for (k v) on kv by #'cddr
          do (setf (gethash k table) v))
    table))

(defun output (data &key (message "OK."))
  (if (and (string= (post/get "browser") "true") (referer))
      (redirect (merge-url (referer) :parameters `(("message" . ,(princ-to-string message)))))
      (api-output data :message message)))

(define-api sensors/measurement/create (type device value &optional time) ()
  (let ((type (ensure-measurement-type (or (ignore-errors (db:ensure-id type)) type)))
        (device (ensure-device (or (ignore-errors (db:ensure-id device)) device)))
        (value (parse-float:parse-float value))
        (time (if time (parse-integer time) (get-universal-time))))
    (output (make-measurement device type value :time time))))

(define-api sensors/measurement/create* (device type[] value[] &optional time) ()
  (let ((device (ensure-device (or (ignore-errors (db:ensure-id device)) device)))
        (time (if time (parse-integer time) (get-universal-time))))
    (output
     (loop for type-string in type[]
           for value-string in value[]
           for type = (ensure-measurement-type (or (ignore-errors (db:ensure-id type-string)) type-string))
           for value = (parse-float:parse-float value-string)
           collect (make-measurement device type value :time time)))))

(define-api sensors/measurement/get (&optional type[] device[] time-start time-stop amount skip) ()
  (output
   (ht :measurements (list-measurements :types (loop for type in type[]
                                                     collect (or (ignore-errors (db:ensure-id type)) type))
                                        :devices (loop for device in device[]
                                                       collect (or (ignore-errors (db:ensure-id device)) device))
                                        :time (when (or time-start time-stop)
                                                (cons (or time-start 0) (or time-stop most-positive-fixnum)))
                                        :amount amount
                                        :skip (or skip 0))
       :types (list-measurement-types))))

(define-api sensors/device/create (name &optional type[]) ()
  (output (apply #'make-device name (or type[] (list-measurement-types)))))

(define-api sensors/device/get () ()
  (output (list-devices)))

(define-api sensors/type/create (name unit min max) ()
  (output (make-measurement-type
               name
               (find-symbol (string-upcase unit) "KEYWORD")
               (parse-float:parse-float min)
               (parse-float:parse-float max))))

(define-api sensors/type/get () ()
  (output (list-measurement-types)))
