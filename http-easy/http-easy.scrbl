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
                     racket/match))

@title{@tt{http-easy}: a high-level HTTP client}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[net/http-easy]

This library wraps @tt{net/http-client} to provide a simpler
interface.  It automatically handles:

@itemlist[
  @item{connection pooling}
  @item{connection timeouts -- WIP}
  @item{SSL verification}
  @item{automatic decompression}
  @item{streaming responses}
  @item{authentication}
  @item{redirect following}
  @item{cookie storage}
]

The following features are currently planned:

@itemlist[
  @item{HTTP proxy support}
  @item{multipart file uploads}
]

The following features may be supported in the future:

@itemlist[
  @item{HTTP/2 and HTTP/3 support}
]

The API is currently in flux so be aware that it may change before the
final release.


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

The input port representing the response body can be accessed using
@racket[response-output]:

@interaction[
#:eval he-eval
(input-port? (response-output res))
(read-string 5 (response-output res))
(read-string 5 (response-output res))
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
      #:auth (auth/basic "Aladdin" "OpenSesame")))
]

And for bearer auth:

@interaction[
#:eval he-eval
(response-json
 (get "https://httpbin.org/bearer"
      #:auth (auth/bearer "secret-api-key")))
]

The above is equivalent to:

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


@section{Reference}

@(define-syntax-rule (defrequester id t ...)
  (defproc (id [uri (or/c bytes? string? url?)]
               [#:close? close? boolean? #f]
               [#:stream? stream? boolean? #f]
               [#:headers headers headers/c (hasheq)]
               [#:params params query-params/c null]
               [#:auth auth (or/c false/c auth-procedure/c) #f]
               [#:data data (or/c false/c bytes? string? input-port? data-procedure/c) #f]
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
                       [#:cookie-jar cookie-jar (or/c false/c (is-a?/c cookie-jar<%>)) #f]) session?]{
  Produces a @racket[session?] value with @racket[pool-config] as its
  connection pool configuration.  Each requested scheme, host and port
  pair has its own connection pool.

  The @racket[ssl-context] argument controls how HTTPS connections are
  handled.  The default implementation verifies TLS certificates,
  verifies hostnames and avoids using weak ciphers.  To use a custom
  certificate chain or private key, you can use
  @racket[ssl-make-client-context].

  The @racket[cookie-jar] argument specifies the cookie jar to use to
  store cookies between requests made against a session.  The default
  is to discard all cookies.  See @racket[list-cookie-jar%].
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
                          [#:data data (or/c false/c bytes? string? input-port? data-procedure/c) #f]
                          [#:form form query-params/c _unsupplied]
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
  has no effect if the @racket[close?] is @racket[#t].

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

  The @racket[data], @racket[form] and @racket[json] arguments can be
  used to send arbitrary data, form-encoded data and JSON data,
  respectively, as part of the request payload.  Only one of the
  arguments may be supplied at a time and providing more than one
  raises a contract error.

  The @racket[max-redirects] argument controls how many redirects are
  followed by the request.  Redirect cycles are not detected.  To
  disable redirect following, set this argument to @racket[0].  The
  @tt{Authorization} header is stripped from redirect requests if the
  target URL does not have the @tech{same origin} as the original
  request.
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
  @defproc[(response-output [r response?]) input-port?]
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

@defproc[(response-json [r response?]) (or/c eof-object? jsexpr?)]{
  Drains @racket[r]'s output port, parses the data as JSON and returns
  it.  An exception is raised if the data is not valid JSON.
}

@defproc[(read-response [r response?]) any/c]{
  Equivalent to @racket[(read (response-output r))].
}

@defproc[(read-response-json [r response?]) (or/c eof-object? jsexpr?)]{
  Equivalent to @racket[(read-json (response-output r))].
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


@subsection{Authentication}

@defthing[auth-procedure/c (-> url? headers/c query-params/c (values headers/c query-params/c))]{
  The contract for auth procedures.  An auth procedure takes the
  current request url, headers and query params and returns a new set
  of headers and query params augmented with authentication
  information.
}

@defproc[(auth/basic [username (or/c bytes? string?)]
                     [password (or/c bytes? string?)]) auth-procedure/c]{

  Generates an auth procedure that authenticates requests using HTTP
  basic auth.
}

@defproc[(auth/bearer [token (or/c bytes? string?)]) auth-procedure/c]{
  Generates an auth procedure that authenticates requests using the
  given bearer @racket[token].
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
                              [#:send send timeout/c 30]) timeout-config?]{

  Produces a timeout config value that can be passed to
  @racket[session-request].

  The @racket[lease] argument controls the maximum amount of time
  leasing a connection from the connection pool can take.

  The @racket[connect] argument controls how long each connection can
  take to connect to the remote end.

  The @racket[send] argument controls how long sending the request
  headers to the remote end can take.
}


@subsection{User Agents}

@defparam[current-user-agent user-agent (or/c bytes? string?)]{
  Holds the value of the @tt{User-Agent} header that is sent with
  every request.
}
