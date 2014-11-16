(asdf:defsystem carrier
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :description "An async HTTP client"
  :depends-on (#:alexandria
               #:babel
               #:fast-io
               #:cl-async 
               #-(or :carrier-no-ssl) #:cl-async-ssl
               #:cl-async-future
               #:quri
               #:fast-http
               #:fast-io)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "carrier" :depends-on ("package" "util"))))

