# Carrier

Carrier is a lightweight, async HTTP client built on top of [cl-async](/orthecreedence/cl-async)
and [fast-http](/fukamachi/fast-http).

Its goal is to allow easy and efficient streaming of data over HTTP. It is the
lightweight cousin to [drakma-async](/orthecreedence/drakma-async).

## Documentation

### request (function)

```lisp
(defun request (url &key (method :get) headers body header-callback body-callback finish-callback))
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
- `header-callback` - A function that is called once the headers from the
*response* are fully parsed. The only argument is a hash table of headers.
- `body-callback` - A function that is called once for each chunk of the HTTP
response body. The function takes three arguments: a byte array, an index
indicating the start of the chunk in the passed byte array, and an index
indicating the end of the chunk in the passed byte array. For instance:
```lisp
:body-callback (lambda (chunk start end)
                 ;; get the body chunk
                 (subseq chunk start end))
```
- `finish-callback` - A function of no arguments that is called once the
response is completely finished downloading. This gets called just before the
returned promise is finished.

## License

MIT!!

