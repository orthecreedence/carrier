(defpackage :carrier
  (:use :cl :blackbird)
  (:import-from :cl-cookie
                :merge-cookies
                :parse-set-cookie-header
                :cookie-jar-host-cookies
                :write-cookie-header)
  (:export #:request))

