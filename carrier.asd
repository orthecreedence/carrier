(asdf:defsystem carrier
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.1"
  :description "An async HTTP client"
  :depends-on (#:alexandria
               #:babel
               #:fast-io
               #:cl-async 
               #-(or :carrier-no-ssl) #:cl-async-ssl
               #:blackbird
               #:quri
               #:fast-http
               #:fast-io)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "carrier" :depends-on ("package" "util"))))

