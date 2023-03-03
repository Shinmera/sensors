(in-package #:sensors)

(defun unit-id (unit)
  (ecase unit
    ((0 :unknown) 0)
    ((1 :celsius) 1)
    ((2 :percent) 2)
    ((3 :ppm) 3)
    ((4 :hpa) 4)
    ((5 :m) 5)))

(defun id-unit (id)
  (ecase id
    (0 :unknown)
    (1 :celsius)
    (2 :percent)
    (3 :ppm)
    (4 :hpa)
    (5 :m)))

(defun make-measurement-type (name unit min max)
  (when (dm:get-one 'measurement-type (db:query (:= 'name name)))
    (error "Type named ~s already exists." name))
  (db:insert 'measurement-type `(("name" . ,name)
                                 ("unit" . ,(unit-id unit))
                                 ("min" . ,(float min 0d0))
                                 ("max" . ,(float max 0d0)))))

(defun ensure-measurement-type (type)
  (etypecase type
    (string
     (or (dm:get-one 'measurement-type (db:query (:= 'name type)))
         (error "No measurement type named ~s" type)))
    (dm:data-model
     (unless (eql 'measurement-type (dm:collection type))
       (error "Not a measurement-type ~s" type))
     type)
    (db:id
     (dm:get-one 'measurement-type (db:query (:= '_id type))))))

(defun list-measurement-types ()
  (dm:get 'measurement-type (db:query :all) :sort '(("name" :asc))))

(defun make-device (name &rest types)
  (db:with-transaction ()
    (when (dm:get-one 'device (db:query (:= 'name name)))
      (error "Device named ~s already exists." name))
    (let ((id (db:insert 'device `(("name" . ,name)))))
      (dolist (type types)
        (db:insert 'supported-type `(("device" . ,id)
                                     ("type" . ,(dm:id (ensure-measurement-type type))))))
      id)))

(defun ensure-device (device)
  (etypecase device
    (string
     (or (dm:get-one 'device (db:query (:= 'name device)))
         (error "No device named ~s" device)))
    (dm:data-model
     (unless (eql 'device (dm:collection device))
       (error "Not a device ~s" device))
     device)
    (db:id
     (dm:get-one 'device (db:query (:= '_id device))))))

(defun list-devices ()
  (dm:get 'device (db:query :all) :sort '(("name" :asc))))

(defun make-measurement (device type value &key (time (get-universal-time)))
  (db:insert 'measurement `(("value" . ,(float value 0d0))
                            ("type" . ,(dm:id (ensure-measurement-type type)))
                            ("device" . ,(dm:id (ensure-device device)))
                            ("time" . ,time))))

(defun list-measurements (&key types devices time amount (skip 0))
  (let ((time (or time '(0 . #.(1- (ash 1 60))))))
    (macrolet ((query (&rest clauses)
                 `(nreverse (dm:get 'measurement (db:query (:and ,@clauses
                                                                 (:>= 'time (car time))
                                                                 (:<= 'time (cdr time))))
                                    :skip skip :amount amount :sort `(("time" :desc))))))
      (cond ((and types devices)
             (query (:in 'type (loop for type in types
                                     collect (dm:id (ensure-measurement-type type))))
                    (:in 'device (loop for device in devices
                                       collect (dm:id (ensure-measurement-type device))))))
            (types
             (query (:in 'type (loop for type in types
                                     collect (dm:id (ensure-measurement-type type))))))
            (devices
             (query (:in 'device (loop for device in devices
                                       collect (dm:id (ensure-measurement-type device))))))
            (T
             (query))))))

(defun last-measurement (device)
  (dm:get-one 'measurement (db:query (:= 'device (dm:id (ensure-device device))))
              :sort `(("time" :desc))))

(defun device-measurement-types (device)
  (dm:get (rdb:join (measurement-type _id) (supported-type type)) (db:query (:= device (dm:id (ensure-device device))))
          :sort `(("name" :asc))))

(define-trigger db:connected ()
  (db:create 'device
             '((name (:varchar 64)))
             :indices '(name))

  (db:create 'measurement-type
             '((name (:varchar 64))
               (unit (:integer 1))
               (max :float)
               (min :float))
             :indices '(name))

  (db:create 'supported-type
             '((device (:id device))
               (type (:id measurement-type)))
             :indices '(device))
  
  (db:create 'measurement
             '((value :float)
               (type (:id measurement-type))
               (device (:id device))
               (time (:integer 8)))
             :indices '(type device time))

  (ignore-errors
   (make-measurement-type "temperature" :celsius -50 +50)
   (make-measurement-type "humidity" :percent 0 100)
   (make-measurement-type "co2" :ppm 200 5000)
   (make-measurement-type "pressure" :hpa 600 1100)
   (make-measurement-type "height" :m 0 2500)))

(defun export-db (file)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (flet ((output (record)
             (format stream "~s~%" (alexandria:hash-table-alist record))))
      (dolist (database '(device measurement-type supported-type measurement))
        (format stream "~s~%" database)
        (db:iterate 'device (db:query :all) #'output)))))

(defun import-db (file)
  (with-open-file (stream file :direction :input)
    (db:with-transaction ()
      (loop with database = NIL
            for expr = (read stream NIL #1='#:eof)
            until (eq expr #1#)
            do (ecase expr
                 (symbol (setf database expr))
                 (list (db:insert database (alexandria:alist-hash-table expr))))))))
