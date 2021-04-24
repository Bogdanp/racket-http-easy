#lang scribble/manual

@(require racket/runtime-path
          racket/sandbox
          scribble/example
          (for-label json
                     net/cookies/user-agent
                     net/http-client
                     net/http-easy
                     net/url
                     openssl
                     racket/base
                     racket/class
                     racket/contract
                     racket/format
                     racket/match
                     xml))

@title{@tt{http-easy}: a high-level HTTP client}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[net/http-easy]

This library wraps @tt{net/http-client} to provide a simple interface
for day-to-day use.  It automatically handles:

@itemlist[
  @item{connection pooling}
  @item{connection timeouts}
  @item{SSL verification}
  @item{automatic compression and decompression}
  @item{streaming responses}
  @item{authentication}
  @item{redirect following}
  @item{cookie storage}
  @item{multipart file uploads}
]


@section{Guide}

@(begin
   (define-syntax-rule (interaction e ...) (examples #:label #f e ...))
   (define-runtime-path log-file "guide-log.rktd")
   (define log-mode (if (getenv "HTTP_EASY_RECORD") 'record 'replay))
   (define (make-he-eval log-file)
     (define ev (make-log-based-eval log-file log-mode))
     (begin0 ev
       (ev '(require racket/contract))))
   (define he-eval (make-he-eval log-file)))

@subsection{Making Requests}

Getting started is as easy as requiring the @tt{net/http-easy} module:

@interaction[
#:eval he-eval
(require net/http-easy)
]

And using one of the built-in requesters to perform a request:

@interaction[
#:eval he-eval
(define res
  (get "https://example.com"))
]

The result is a @racket[response?] value that you can inspect:

@interaction[
#:eval he-eval
(response-status-code res)
(response-status-message res)
(response-headers-ref res 'date)
(subbytes (response-body res) 0 30)
]

Connections to remote servers are automatically pooled so closing the
response returns its underlying connection to the pool:

@interaction[
#:eval he-eval
(response-close! res)
]

@(define sr (secref "guide:streaming"))

If you forget to manually close a response, its underlying connection
will get returned to the pool when the response gets
garbage-collected.  Unless you explicitly use @sr, you don't have to
worry about this much.

@subsection[#:tag "guide:streaming"]{Streaming Responses}

Response bodies can be streamed by passing @racket[#t] as the
@racket[#:stream?] argument to any of the requesters:

@interaction[
#:eval he-eval
(define res
  (get "https://example.com" #:stream? #t))
]

The input port containing the response body can be accessed using
@racket[response-output]:

@interaction[
#:eval he-eval
(input-port? (response-output res))
(read-string 5 (response-output res))
(read-string 5 (response-output res))
]

Using @racket[response-body] immediately drains the remaining data and
closes the input port:

@interaction[
#:eval he-eval
(subbytes (response-body res) 0 10)
(subbytes (response-body res) 0 20)
(port-closed? (response-output res))
]

@subsection{Authenticating Requests}

The library provides an auth procedure for HTTP basic auth:

@interaction[
#:eval he-eval
(response-status-line
 (get "https://httpbin.org/basic-auth/Aladdin/OpenSesame"))
]

@interaction[
#:eval he-eval
(response-json
 (get "https://httpbin.org/basic-auth/Aladdin/OpenSesame"
      #:auth (basic-auth "Aladdin" "OpenSesame")))
]

And for bearer auth:

@interaction[
#:eval he-eval
(response-json
 (get "https://httpbin.org/bearer"
      #:auth (bearer-auth "secret-api-key")))
]

The above example is equivalent to:

@interaction[
#:eval he-eval
(response-json
 (get "https://httpbin.org/bearer"
      #:auth (lambda (uri headers params)
               (values (hash-set headers 'authorization "Bearer secret-api-key") params))))
]

@subsection{Sending Data}

You can supply a list of pairs to be sent as an
@tt{application/x-www-form-urlencoded} payload:

@interaction[
#:eval he-eval
(define res
 (response-json
  (post "https://httpbin.org/post"
        #:form '((a . "hello")
                 (b . "there")))))
(hash-ref res 'form)
]

Alternatively, you can supply the @racket[#:json] keyword argument to
send an @tt{application/json} payload:

@interaction[
#:eval he-eval
(define res
 (response-json
  (post "https://httpbin.org/anything"
        #:json (hasheq 'a "hello"
                       'b "there"))))
(hash-ref res 'json)
]

To send data using arbitrary formats, you can use the @racket[#:data]
keyword argument:

@interaction[
#:eval he-eval
(define res
 (response-json
  (post "https://httpbin.org/anything"
        #:data #"hello")))
(hash-ref res 'data)
]

To gzip the payload, use the @racket[gzip-payload] combinator:

@interaction[
#:eval he-eval
(define res
 (response-json
  (post "https://httpbin.org/anything"
        #:data (gzip-payload (pure-payload #"hello")))))
(hash-ref res 'data)
]

@interaction[
#:eval he-eval
(define res
 (response-json
  (post "https://httpbin.org/anything"
        #:data (gzip-payload (json-payload (hasheq 'hello "world"))))))
(hash-ref res 'data)
]


@subsection{Cookie Storage}

To store cookies between requests pass a @racket[cookie-jar<%>] into
your @racket[session?]:

@interaction[
#:eval he-eval
(require net/cookies
         net/url
         racket/class)

(code:line)
(define jar (new list-cookie-jar%))
(define session-with-cookies
  (make-session #:cookie-jar jar))

(code:line)
(parameterize ([current-session session-with-cookies])
  (get "https://httpbin.org/cookies/set/hello/world")
  (response-json (get "https://httpbin.org/cookies")))

(code:line)
(for ([c (in-list (send jar cookies-matching (string->url "https://httpbin.org")))])
  (printf "~a: ~a" (ua-cookie-name c) (ua-cookie-value c)))
]


@subsection{UNIX Sockets}

To make a request to a UNIX domain socket, pass @tt{http+unix} as the
scheme and url-encode the path to the socket as the host.

@interaction[
#:eval he-eval
(response-status-code (get "http+unix://%2Fvar%2Frun%2Fdocker.sock/info"))
]


@section{Reference}

@(define-syntax-rule (defrequester id t ...)
  (defproc (id [uri (or/c bytes? string? url?)]
               [#:close? close? boolean? #f]
               [#:stream? stream? boolean? #f]
               [#:headers headers headers/c (hasheq)]
               [#:params params query-params/c null]
               [#:auth auth (or/c false/c auth-procedure/c) #f]
               [#:data data (or/c false/c bytes? string? input-port? payload-procedure/c) #f]
               [#:form form query-params/c _unsupplied]
               [#:json json jsexpr? _unsupplied]
               [#:timeouts timeouts timeout-config? (make-timeout-config)]
               [#:max-attempts max-attempts exact-positive-integer? 3]
               [#:max-redirects max-redirects exact-nonnegative-integer? 16]
               [#:user-agent user-agent (or/c bytes? string?) (current-user-agent)]) response? t ...))

@deftogether[(
  @defrequester[get]
  @defrequester[post]
  @defrequester[delete]
  @defrequester[head]
  @defrequester[options]
  @defrequester[patch]
  @defrequester[put]
)]{
  Requesters for each of the standard HTTP methods.  See
  @racket[session-request] for a description of the individual
  arguments.
}


@subsection{Sessions}

@deftogether[(
  @defthing[method/c (or/c 'delete 'head 'get 'options 'patch 'post 'put symbol?)]
  @defthing[headers/c (hash/c symbol? (or/c bytes? string?))]
  @defthing[form-data/c (listof (cons/c symbol? (or/c false/c string?)))]
  @defthing[query-params/c (listof (cons/c symbol? (or/c false/c string?)))]
)]

@defparam[current-session session session? #:value (make-session)]{
  Holds the current session that is used by the @racket[delete],
  @racket[head], @racket[get], @racket[options], @racket[patch],
  @racket[post] and @racket[put] requesters.
}

@defproc[(session? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a session value.
}

@defproc[(make-session [#:pool-config pool-config pool-config? (make-pool-config)]
                       [#:ssl-context ssl-context ssl-client-context? (ssl-secure-client-context)]
                       [#:cookie-jar cookie-jar (or/c false/c (is-a?/c cookie-jar<%>)) #f]
                       [#:proxies proxies (listof proxy?) null]) session?]{
  Produces a @racket[session?] value with @racket[#:pool-config] as
  its connection pool configuration.  Each requested scheme, host and
  port pair has its own connection pool.

  The @racket[#:ssl-context] argument controls how HTTPS connections
  are handled.  The default implementation verifies TLS certificates,
  verifies hostnames and avoids using weak ciphers.  To use a custom
  certificate chain or private key, you can use
  @racket[ssl-make-client-context].

  The @racket[#:cookie-jar] argument specifies the cookie jar to use
  to store cookies between requests made against a session.  The
  default is to discard all cookies.  See @racket[list-cookie-jar%].

  The @racket[#:proxies] argument specifies an optional list of
  @tech{proxies} to use when making requests.

  @history[#:changed "0.3" @elem{Added the @racket[#:proxies] argument.}]
}

@defproc[(session-close! [s session?]) void?]{
  Closes @racket[s] and all of its associated connections and responses.
}

@defproc[(session-request [s session?]
                          [uri (or/c bytes? string? url?)]
                          [#:close? close? boolean? #f]
                          [#:stream? stream? boolean? #f]
                          [#:method method method/c 'get]
                          [#:headers headers headers/c (hasheq)]
                          [#:params params query-params/c null]
                          [#:auth auth (or/c false/c auth-procedure/c) #f]
                          [#:data data (or/c false/c bytes? string? input-port? payload-procedure/c) #f]
                          [#:form form form-data/c _unsupplied]
                          [#:json json jsexpr? _unsupplied]
                          [#:timeouts timeouts timeout-config? (make-timeout-config)]
                          [#:max-attempts max-attempts exact-positive-integer? 3]
                          [#:max-redirects max-redirects exact-nonnegative-integer? 16]
                          [#:user-agent user-agent (or/c bytes? string?) (current-user-agent)]) response?]{

  Requests @racket[uri] using @racket[s]'s connection pool and
  associated settings (SSL context, proxy, cookie jar, etc.).

  Response values returned by this function must be closed before
  their underlying connection is returned to the pool.  If the
  @racket[close?] argument is @racket[#t], this is done
  automatically.  Ditto if the responses are garbage-collected.

  If the @racket[close?] argument is @racket[#t], then the response's
  output port is drained and the connection is closed.

  If the @racket[stream?] argument is @racket[#f] (the default), then
  the response's output port is drained and the resulting byte string
  is stored on the response value.  The drained data is accessible
  using the @racket[response-body] function.  If the argument is
  @racket[#t], then the response body is streamed and the data is
  accessible via the @racket[response-output] function.  This argument
  has no effect when @racket[close?] is @racket[#t].

  The @racket[method] argument specifies the HTTP request method to use.

  Query parameters may be specified directly on the @racket[uri]
  argument or via the @racket[params] argument.  If query parameters
  are specified via both arguments, then the list of @racket[params]
  is appended to those already in the @racket[uri].

  The @racket[auth] argument allows authentication headers and query
  params to be added to the request.  When following redirects, the
  auth procedure is applied to subsequent requests only if the target
  URL has the @tech{same origin} as the original request.  Two URLs
  are considered to have the @deftech{same origin} if their scheme,
  hostname and port are the same.

  The @racket[data] argument can be used to send arbitrary request
  data to the remote end.  A number of @tech{payload procedures}
  are available for producing data in standard formats:

  @interaction[
  #:eval he-eval
  (define res
    (post #:data (json-payload (hasheq 'hello "world"))
          "https://httpbin.org/post"))
  (hash-ref (response-json res) 'data)
  ]

  The @racket[form] argument is a shorthand for passing a
  @racket[form-payload] as the @racket[data] argument.

  The @racket[json] argument is a shorthand for passing a
  @racket[json-payload] as the @racket[data] argument.

  The @racket[data], @racket[form] and @racket[json] arguments are
  mutually-exclusive.  Supplying more than one at a time causes a
  contract error to be raised.

  The @racket[timeouts] argument controls how long various aspects of
  the request cycle will be waited on.  When a timeout is exceeded, an
  @racket[exn:fail:http-easy:timeout?] error is raised.  When
  redirects are followed, the timeouts are per request.

  The @racket[max-attempts] argument controls how many times
  connection errors are retried.  This meant to handle connection
  resets and the like and isn't a general retry mechanism.

  The @racket[max-redirects] argument controls how many redirects are
  followed by the request.  Redirect cycles are not detected.  To
  disable redirect following, set this argument to @racket[0].  The
  @tt{Authorization} header is stripped from redirect requests if the
  target URL does not have the @tech{same origin} as the original
  request.

  @history[#:changed "0.3" @elem{Added support for the @tt{http+unix}
  scheme to allow requests to UNIX domain sockets.}]
}


@subsection{Responses}

@defthing[status-code/c (integer-in 100 999)]{
  The contract for HTTP status codes.
}

@defproc[(response? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a response.
}

@defform[
  (response clause ...)
  #:grammar ([clause (code:line #:status-line e)
                     (code:line #:status-code e)
                     (code:line #:status-message e)
                     (code:line #:http-version e)
                     (code:line #:history e)
                     (code:line #:headers heads maybe-rest)
                     (code:line #:body e)
                     (code:line #:json e)]
             [heads ([header-id e] ...)]
             [maybe-rest (code:line)
                         e])]{

  A match expander for @racket[response?] values.

  @interaction[
  #:eval he-eval
  (require racket/match)

  (code:line)
  (match (get "https://example.com")
   [(response
     #:status-code 200
     #:headers ([content-type (and (regexp #"text/html") the-content-type)]))
    the-content-type])
  ]
}

@deftogether[(
  @defproc[(response-status-line [r response?]) bytes?]
  @defproc[(response-http-version [r response?]) bytes?]
  @defproc[(response-status-code [r response?]) status-code/c]
  @defproc[(response-status-message [r response?]) bytes?]
  @defproc[(response-headers [r response?]) (listof bytes?)]
)]{
  Accessors for the raw data available on a response.
}

@defproc[(response-headers-ref [r response?]
                               [h symbol?]) (or/c false/c bytes?)]{

  Looks up the first response header whose name is @racket[h].  Header
  names are normalized to lower case.
}

@defproc[(response-headers-ref* [r response?]
                                [h symbol?]) (listof bytes?)]{

  Looks up all the response headers whose names are @racket[h].  As in
  @racket[response-headers-ref], the names are all normalized to lower
  case.
}

@defproc[(response-history [r response?]) (listof response?)]{
  When redirects are followed, the trail of redirected responses is
  preserved in each individual response.  The responses are sorted
  reverse chronologically.
}

@defproc[(response-body [r response?]) bytes?]{
  Drains @racket[r]'s output port and returns the result as a byte
  string.
}

@defproc[(response-output [r response?]) input-port?]{
  Returns a port which contains the contents of the response.  If
  @racket[response-body] has already been called on the response, then
  the port is closed.  Likewise, if either @racket[#:close? #t] or
  @racket[#:stream? #f] were passed to @racket[session-request], then
  the response data is only accessible via @racket[response-body].
  See the @sr section of the guide for an example.
}

@defproc[(response-json [r response?]) (or/c eof-object? jsexpr?)]{
  Drains @racket[r]'s output port, parses the data as JSON and returns
  it.  An exception is raised if the data is not valid JSON.
}

@defproc[(response-xexpr [r response?]) xexpr?]{
  Drains @racket[r]'s output port, parses the data as an @racket[xexpr?]
  and returns it. An exception is raised if the data is not valid XML.
}

@defproc[(response-xml [r response?]) document?]{
  Drains @racket[r]'s output port, parses the data as an XML
  @racket[document?]  and returns it. An exception is raised if the
  data is not valid XML.
}

@defproc[(read-response [r response?]) any/c]{
  Equivalent to @racket[(read (response-output r))].
}

@defproc[(read-response-json [r response?]) (or/c eof-object? jsexpr?)]{
  Equivalent to @racket[(read-json (response-output r))].
}

@defproc[(read-response-xexpr [r response?]) xexpr?]{
  Equivalent to @racket[(xml->xexpr (document-element (read-response-xml r)))].
}

@defproc[(read-response-xml [r response?]) document?]{
  Equivalent to @racket[(read-xml/document (response-output r))].
}

@defproc[(response-drain! [r response?]) void?]{
  Drains @racket[r]'s output port.
}

@defproc[(response-close! [r response?]) void?]{
  Closes @racket[r] and returns its underlying connection to the pool.
}


@subsection{Connection Pooling}

@defthing[limit/c (or/c +inf.0 exact-positive-integer?)]{
  The contract for limit values.
}

@defproc[(pool-config? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a pool config.
}

@defproc[(make-pool-config [#:max-size max-size limit/c 128]
                           [#:idle-timeout idle-timeout timeout/c 600]) pool-config?]{

  Produce a pool config values that can be passed to
  @racket[make-session].

  The @racket[max-size] argument controls the maximum number of
  connections in a pool.  Once a pool reaches this size, leasing a
  connection blocks until one is available or until the @tt{lease}
  timeout is reached.

  The @racket[idle-timeout] argument controls the amount of time idle
  connections are kept open for.
}


@subsection{Proxies}

@deftech{Proxies} tunnel requests to one host through another.

@defproc[(proxy? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{proxy}.
}

@defproc[(make-proxy [matches? (-> url? boolean?)]
                     [connect! (-> http-conn? url? (or/c #f ssl-client-context?) void?)]) proxy?]{
  Returns a new @tech{proxy} that applies to requests whose URL
  @racket[matches?] returns @racket[#t] for.

  @history[#:added "0.3"]
}

@defproc[(make-http-proxy [proxy-url (or/c bytes? string? url?)]
                          [matches? (-> url? boolean?) (λ (u) (equal? (url-scheme u) "http"))]) proxy?]{
  Returns an HTTP @tt{CONNECT} @racket[proxy?] that tunnels requests
  whose URLs @racket[matches?] is @racket[#t] for through the server
  at @racket[proxy-url].

  @history[#:added "0.3"]
}

@defproc[(make-https-proxy [proxy-url (or/c bytes? string? url?)]
                           [matches? (-> url? boolean?) (λ (u) (equal? (url-scheme u) "https"))]) proxy?]{
  Returns an HTTPS @tt{CONNECT} @racket[proxy?] that tunnels requests
  whose URLs @racket[matches?] is @racket[#t] for through the server
  at @racket[proxy-url].

  @history[#:added "0.3"]
}


@subsection{Authentication}

@defthing[auth-procedure/c (-> url? headers/c query-params/c (values headers/c query-params/c))]{
  The contract for auth procedures.  An auth procedure takes the
  current request url, headers and query params and returns a new set
  of headers and query params augmented with authentication
  information.
}

@defproc[(basic-auth [username (or/c bytes? string?)]
                     [password (or/c bytes? string?)]) auth-procedure/c]{

  Generates an auth procedure that authenticates requests using HTTP
  basic auth.
}

@defproc[(bearer-auth [token (or/c bytes? string?)]) auth-procedure/c]{
  Generates an auth procedure that authenticates requests using the
  given bearer @racket[token].
}


@subsection{Payload Procedures}

@deftech{Payload procedures} produce data and associated headers to be
sent to a remote server.

@defthing[payload-procedure/c (-> headers/c (values headers/c (or/c bytes? string? input-port?)))]{
  The contract for payload procedures.  A payload procedure takes the
  current set of request headers and returns new request headers and a
  value to be used as the request body.
}

@defproc[(form-payload [v form-data/c]) payload-procedure/c]{
  Produces a payload procedure that encodes @racket[v] as form data
  using the @tt{application/x-www-form-urlencoded} content type.
}

@defproc[(json-payload [v jsexpr?]) payload-procedure/c]{
  Produces a payload procedure that encodes @racket[v] as JSON data.
}

@defproc[(gzip-payload [p payload-procedure/c]) payload-procedure/c]{
  Produces a payload procedure that gzips the output of @racket[p].
}

@defproc[(pure-payload [v (or/c bytes? string? input-port?)]) payload-procedure/c]{
  Produces a payload procedure that uses @racket[v] as the request body.
}

@defproc[(part? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @racket[multipart-payload] part.
}

@defproc[(field-part [name (or/c bytes? string?)]
                     [value (or/c bytes? string?)]
                     [content-type (or/c bytes? string?) #"text/plain"]) part?]{

  Produces a @racket[part?] for use with @racket[multipart-payload]
  that encapsulates a form field.
}

@defproc[(file-part [name (or/c bytes? string?)]
                    [inp input-port?]
                    [filename (or/c bytes? string?) (~a (object-name inp))]
                    [content-type (or/c bytes? string?) #"application/octet-stream"]) part?]{

  Produces a @racket[part?] for use with @racket[multipart-payload]
  that encapsulates a file.
}

@defproc[(multipart-payload [f part?] ...
                            [#:boundary boundary (or/c bytes? string?) _unsupplied]) payload-procedure/c]{

  Produces a @tt{multipart/form-data} payload.

  @interaction[
  #:eval he-eval
  (define resp
    (post
     #:data (multipart-payload
             (field-part "a" "hello")
             (file-part "f" (open-input-string "hello world!")))
     "https://httpbin.org/anything"))
  (hash-ref (response-json resp) 'form)
  (hash-ref (response-json resp) 'files)
  ]
}


@subsection{Timeouts}

@defthing[timeout/c (or/c false/c (and/c real? positive?))]{
  The contract for timeout values.  All timeout values represent seconds.
}

@defproc[(timeout-config? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a timeout config value.
}

@defproc[(make-timeout-config [#:lease lease timeout/c 5]
                              [#:connect connect timeout/c 5]
                              [#:request request timeout/c 30]) timeout-config?]{

  Produces a timeout config value that can be passed to
  @racket[session-request].

  The @racket[lease] argument controls the maximum amount of time
  leasing a connection from the connection pool can take.

  The @racket[connect] argument controls how long each connection can
  take to connect to the remote end.

  The @racket[request] argument controls how long to wait on a request
  before its response headers are returned.
}


@subsection{Errors}

@deftogether[(
  @defproc[(exn:fail:http-easy? [v any/c]) boolean?]
  @defproc[(exn:fail:http-easy:timeout? [v any/c]) boolean?]
  @defproc[(exn:fail:http-easy:timeout-kind [e exn:fail:http-easy:timeout?]) (or/c 'lease 'connect 'request)]
)]


@subsection{User Agents}

@defparam[current-user-agent user-agent (or/c bytes? string?)]{
  Holds the value of the @tt{User-Agent} header that is sent with
  every request.
}
