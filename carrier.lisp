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
                    header-callback
                    body-callback
                    finish-callback)
  "Make an HTTP request."
  (let* ((parsed (quri:uri url))
         (future (make-future))
         (http (fast-http:make-http-response))
         (sock nil)
         (our-finish-callback (lambda ()
                                (unless (as:socket-closed-p sock)
                                  (as:close-socket sock))
                                (funcall finish-callback)
                                (finish future)))
         (parser (fast-http:make-parser http
                                        :header-callback header-callback
                                        :body-callback body-callback
                                        :finish-callback our-finish-callback))
         (request-data (build-request parsed method headers body)))
    (setf sock (cl-async:tcp-connect
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

