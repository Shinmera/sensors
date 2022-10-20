(asdf:defsystem #:sensors
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :version "0.0.0"
  :serial T
  :components ((:file "module")
               (:file "db")
               (:file "api")
               (:file "frontend"))
  :depends-on (:i-sqlite
               (:interface :relational-database)
               :r-data-model
               :r-clip
               :i-json
               :cl-smtp
               :parse-float))
