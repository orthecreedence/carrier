(in-package :carrier)

(define-condition http-eof (as:event-error) ()
  (:report (lambda (c s) (format s "HTTP connection EOF: ~a: ~a" (as:event-errcode c) (as:event-errmsg c))))
  (:documentation "Triggered when an HTTP peer closes the connection."))

(define-condition http-timeout (as:event-error) ()
  (:report (lambda (c s) (format s "HTTP connection timeout: ~a: ~a" (as:event-errcode c) (as:event-errmsg c))))
  (:documentation "Triggered when an HTTP connection times out."))

(defun char-up (char)
  "Turn a character uppercase."
  (aref (string-upcase (string char)) 0))

(defun header-capitalize (key)
  "Turn :content-type into \"Content-Type\"."
  (let ((str (string-downcase (string key))))
    (dotimes (i (length str))
      (when (or (= i 0)
                (and (not (zerop i))
                     (eq (aref str (1- i)) #\-)))
        (setf (aref str i) (char-up (aref str i)))))
    str))

(defun build-request (parsed-uri method headers body)
  "Build an HTTP request."
  (let ((nl (format nil "~c~c" #\return #\newline))
        (method (string-upcase (string method)))
        (headers (if (hash-table-p headers)
                     (alexandria:hash-table-plist headers)
                     headers))
        (body (when body
                (if (stringp body)
                    (babel:string-to-octets body)
                    body))))
    (unless (getf headers :host)
      (setf (getf headers :host) (quri:uri-host parsed-uri)))
    (when body
      (setf (getf headers :content-length) (length body)))
    (with-output-to-string (s)
      (format s "~a ~a HTTP/1.1~a" method (quri:uri-path parsed-uri) nl)
      (loop for (k v) on headers by #'cddr do
        (let ((key (header-capitalize k)))
          (format s "~a: ~a~a" key v nl)))
      (format s "~a" nl)
      (when body
        (format s "~a" (babel:octets-to-string body))))))

(defun request (url &key
                    (method :get)
                    headers
                    body
                    store-body
                    header-callback
                    body-callback
                    finish-callback)
  "Make an HTTP request."
  (let* ((parsed (quri:uri url))
         (future (make-future))
         (http (fast-http:make-http-response))
         (sock nil)
         (body-buffer (fast-io:make-output-buffer))
         (response-headers nil)
         (our-header-callback (lambda (headers)
                                (setf response-headers headers)
                                (when header-callback
                                  (funcall header-callback headers))))
         (our-body-callback (lambda (chunk start end)
                              (when store-body
                                (fast-io:fast-write-sequence chunk body-buffer start end))
                              (when body-callback
                                (funcall body-callback chunk start end))))
         (our-finish-callback (lambda ()
                                (unless (as:socket-closed-p sock)
                                  (as:close-socket sock))
                                (when finish-callback
                                  (funcall finish-callback))
                                (let ((body (when store-body
                                              (fast-io:finish-output-buffer body-buffer)))
                                      (status (fast-http:http-status http)))
                                  (finish future body status response-headers))))
         (parser (fast-http:make-parser http
                                        :header-callback our-header-callback
                                        :body-callback our-body-callback
                                        :finish-callback our-finish-callback))
         (request-data (build-request parsed method headers body))
         (connect-fn (if (string= (quri:uri-scheme parsed) "https")
                         'as-ssl:tcp-ssl-connect
                         'as:tcp-connect)))
    (setf sock (funcall connect-fn
                 (quri:uri-host parsed)
                 (or (quri:uri-port parsed) 80)
                 (lambda (sock data)
                   (declare (ignore sock))
                   (funcall parser data))
                 (lambda (ev)
                   (handler-case (error ev)
                     (as:tcp-eof ()
                       (funcall parser :eof)
                       (signal-error future (make-instance 'http-eof
                                                           :code -1
                                                           :msg "HTTP stream client peer closed connection.")))
                     (as:tcp-timeout ()
                       (signal-error future (make-instance 'http-timeout
                                                           :code -1
                                                           :msg "HTTP stream client timed out.")))
                     (t ()
                       (signal-error future ev))))
                 :data request-data))
    future))

