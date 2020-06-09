#lang scribble/manual

@(require racket/runtime-path
          racket/sandbox
          scribble/example
          (for-label json
                     net/http-easy
                     net/url
                     openssl
                     racket/base
                     racket/contract))

@title{@tt{http-easy}: a high-level HTTP client}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[net/http-easy]

This library wraps @tt{net/http-client} to provide a simpler
interface.  It automatically handles:

@itemlist[
  @item{connection pooling}
  @item{connection timeouts -- WIP}
  @item{SSL verification}
  @item{digest authentication}
  @item{cookie storage}
  @item{file uploads}
  @item{redirect following}
  @item{streaming responses}
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

If you forget to manually close a response, its underlying connection
will get returned to the pool when the response gets
garbage-collected.

@subsection{Streaming Responses}

Response bodies can be streamed by passing @racket[#f] as the
@racket[#:drain?] argument to any of the requesters:

@interaction[
#:eval he-eval
(define res
  (get "https://example.com" #:drain? #f))
]

The input port representing the response body can be accessed using
@racket[response-output]:

@interaction[
#:eval he-eval
(input-port? (response-output res))
(read-string 5 (response-output res))
(read-string 5 (response-output res))
]


@section{Reference}

@(define-syntax-rule (defrequester id t ...)
  (defproc (id [uri (or/c bytes? string? url?)]
               [#:drain? drain? boolean? #t]
               [#:close? close? boolean? #f]
               [#:headers headers (hash/c symbol? (or/c bytes? string?)) (hasheq)]
               [#:params params (listof (cons/c symbol? (or/c false/c string?))) null]
               [#:data data (or/c false/c bytes? string? input-port?) #f]
               [#:timeouts timeouts timeout-config? (make-timeout-config)]
               [#:max-attempts max-attempts exact-positive-integer? 3]
               [#:max-redirects max-redirects exact-nonnegative-integer? 16]) response? t ...))

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

@defparam[current-session session session? #:value (make-session)]{
  Holds the current session that is used by the @racket[delete],
  @racket[head], @racket[get], @racket[options], @racket[patch],
  @racket[post] and @racket[put] requesters.
}

@defproc[(session? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a session value.
}

@defproc[(make-session [#:pool-config pool-config pool-config? (make-pool-config)]
                       [#:ssl-context ssl-context ssl-client-context? (ssl-secure-client-context)]) session?]{
  Produces a @racket[session?] value with @racket[pool-config] as its
  connection pool configuration.  Each requested scheme, host and port
  pair has its own connection pool.

  The @racket[ssl-context] argument controls how HTTPS connections are
  handled.  The default implementation verifies TLS certificates,
  verifies hostnames and avoids using weak ciphers.  To use a custom
  certificate chain or private key, you can use
  @racket[ssl-make-client-context].
}

@defproc[(session-close! [s session?]) void?]{
  Closes @racket[s] and all of its associated connections and responses.
}

@defproc[(session-request [s session?]
                          [uri (or/c bytes? string? url?)]
                          [#:drain? drain? boolean? #t]
                          [#:close? close? boolean? #f]
                          [#:method method symbol? 'get]
                          [#:headers headers (hash/c symbol? (or/c bytes? string?)) (hasheq)]
                          [#:params params (listof (cons/c symbol? (or/c false/c string?))) null]
                          [#:data data (or/c false/c bytes? string? input-port?) #f]
                          [#:timeouts timeouts timeout-config? (make-timeout-config)]
                          [#:max-attempts max-attempts exact-positive-integer? 3]
                          [#:max-redirects max-redirects exact-nonnegative-integer? 16]) response?]{

  Requests @racket[uri] using @racket[s]'s connection pool.

  Response values returned by this function must be closed before
  their underlying connection is returned to the pool.  If the
  @racket[close?] argument is @racket[#t], this is done
  automatically.  Ditto if the responses are garbage-collected.

  If the @racket[drain?] argument is @racket[#t], then the response's
  output port is drained and the resulting byte string is stored on
  the response value.  The drained data is accessible using the
  @racket[response-body] function.

  The @racket[method] argument specifies the HTTP request method to use.

  Query parameters may be specified directly on the @racket[uri]
  argument or via the @racket[params] argument.  If query parameters
  are specified via both arguments, then the list of @racket[params]
  is appended to those already in the @racket[uri].

  The @racket[max-redirects] argument controls how many redirects are
  followed by the request.  Redirect cycles are not currently
  detected.  To disable redirect following, set this argument to
  @racket[0].
}


@subsection{Responses}

@defthing[status-code/c (integer-in 100 999)]{
  The contract for HTTP status codes.
}

@defproc[(response? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a response.
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

@defproc[(response-body [r response?]) bytes?]{
  Drains @racket[r]'s output port and returns the result as a byte
  string.
}

@defproc[(response-json [r response?]) jsexpr?]{
  Drains @racket[r]'s output port, parses the data as JSON and returns
  it.  An exception is raised if the data is not valid JSON.
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
