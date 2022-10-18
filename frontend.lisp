(in-package #:sensors)

(defun render-page (page &rest args)
  (r-clip:with-clip-processing (page)
    (apply #'r-clip:process T
           :title (config :title)
           :page page
           :content (plump:parse content)
           :copyright (config :copyright)
           :version (asdf:component-version (asdf:find-system :courier))
           :logged-in (user:check (auth:current "anonymous") (perm courier))
           :registration-open (config :registration-open)
           args)))

(define-page dashboard "sensors/" ()
  (r-clip:with-clip-processing ("dashboard.ctml" "text/html")
    (r-clip:process T :devices (list-devices))))

(defun format-value (value)
  (format NIL "~,1f" value))
