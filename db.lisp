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

(defun list-measurements (&key type time amount (skip 0))
  (cond ((and (null type) (null time))
         (nreverse (dm:get 'measurement (db:query :all)
                           :skip skip :amount amount :sort `(("time" :desc)))))
        ((and time type)
         (nreverse (dm:get 'measurement (db:query (:and (:= 'type (dm:id (ensure-measurement-type type)))
                                                        (:>= 'time (car time))
                                                        (:<= 'time (cdr time))))
                           :skip skip :amount amount :sort `(("time" :desc)))))
        (time
         (nreverse (dm:get 'measurement (db:query (:and (:>= 'time (car time))
                                                        (:<= 'time (cdr time))))
                           :skip skip :amount amount :sort `(("time" :desc)))))
        (type
         (nreverse (dm:get 'measurement (db:query (:= 'type (dm:id (ensure-measurement-type type))))
                           :skip skip :amount amount :sort `(("time" :desc)))))))

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
