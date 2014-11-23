# Carrier

Carrier is a lightweight, async HTTP client built on top of [cl-async](/orthecreedence/cl-async)
and [fast-http](/fukamachi/fast-http).

Its goal is to allow easy and efficient streaming of data over HTTP. It is the
lightweight cousin to [drakma-async](/orthecreedence/drakma-async).

## Documentation

### request (function)

```lisp
(defun request (url &key (method :get) headers body return-body header-callback body-callback finish-callback (redirect 5) redirect-non-get timeout))
  => promise
```

Perform an HTTP request. Returns a promise (to be used with
[cl-async-future](/orthecreedence/cl-async-future)) that is finished when the
response has fully downloaded.

- `url` - the URL we're requesting.
- `method` - a keyword method (`:get`, `:post`, etc). Defaults to `:get`.
- `headers` - a hash table or plist of headers to set with the request. Note
that the "Host" header is set automatically (if not proveded) and if the `body`
argument is passed, then "Content-Length" is set as well.
- `body` - A string or byte array to send as the HTTP body. If present, will set
the "Content-Length" header automatically in the request.
- `store-body` - If T, will store the entire HTTP response body and finish the
returned promise with it once complete. If this is left `nil`, then the first
value of the finished promise will be `nil`.
- `header-callback` - A function that is called once the headers from the
*response* are fully parsed. The only argument is a hash table of headers.
```lisp
(lambda (headers) (gethash "content-type" headers))
```
- `body-callback` - A function that is called once for each chunk of the HTTP
response body. The function takes three arguments: a byte array, an index
indicating the start of the chunk in the passed byte array, and an index
indicating the end of the chunk in the passed byte array. For instance:
```lisp
(lambda (chunk start end)
  ;; here's how you'd get the actual bytes. note that most stream functions
  ;; take :start and :end functions, so we don't actually have to do a subseq
  ;; to send the chunk where we need it to go
  (subseq chunk start end))
```
- `finish-callback` - A function of no arguments that is called once the
response is completely finished downloading. This gets called just before the
returned promise is finished.
```lisp
(lambda () ...)
```
- `redirect` - Either nil or an integer value telling Carrier how many redirects
(if any) to follow before completing. Default is `5`.
- `redirect-non-get` - This argument determines whether or not we redirect when
performing a request other than a `:get` or `:head`. If `t`, we just redirect
blindly until either we hit our destination *or* the max number of redirects
(set by the `:redirect` arg) is reached. However, `:redirect-non-get` can also
be a function of two arguments (the redirecting URL given in the `Location`
header and the response headers hash table).
```lisp
(lambda (redirecting-url headers) ...)
```
- `timeout` - How many seconds to wait for the socket to start reading. If `nil`
(the default), uses the OS default.

## License

MIT!!

