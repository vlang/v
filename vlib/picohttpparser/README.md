## Description

Prefer `import pico_http_parser` for new code. The original
`import picohttpparser` path remains available for compatibility.

`picohttpparser` is a pure V implementation of
[picohttpparser](https://github.com/h2o/picohttpparser),
which in turn is "a tiny, primitive, fast HTTP request/response parser."

The upstream C source is no longer vendored; the module carries no third-party
C dependency. Only a thin `C.send` interop call remains in `response.c.v` for
SIGPIPE-safe socket writes.

Header names accept ASCII letters, digits, and the HTTP token punctuation
`!#$%&'*+-.^_` followed by backtick, `|`, and `~`. Spaces, other separators, control bytes,
and non-ASCII bytes are rejected. A colon separates the header name from its value.

Request parsing accepts one leading CRLF and counts it in the consumed byte total.
