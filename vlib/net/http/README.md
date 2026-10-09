# net.http

`net.http` is V's HTTP client and server. The client speaks HTTP/1.1 and
HTTP/2, with HTTP/3 available behind `-d http3`; the server speaks HTTP/1.1 and,
opt-in, HTTP/2 — over plain TCP or with TLS termination.

For building web applications on top of it, see [`veb`](../../veb), which has its
own server and router.

## Status reason phrases

`http.status_from_int(code).str()` returns the standard HTTP reason phrase, matching
[Go's StatusText](https://go.dev/src/net/http/status.go). Unknown and unassigned codes return
an empty string, including legacy nonstandard statuses 306 and 509. Servers retain the numeric
status code when the reason phrase is empty, and an explicit custom reason phrase is preserved.

The existing enum identifiers `checkpoint_draft` and `unordered_collection` remain available
for compatibility. Their standard phrases are `Early Hints` (103) and `Too Early` (425).
Status 203 uses `Non-Authoritative Information`, 207 uses `Multi-Status`, and 418 uses
`I'm a teapot`.

## Making requests

```v ignore
import net.http

resp := http.get('https://example.com/')!
println(resp.status_code)
println(resp.body)
```

`http.post`, `http.post_json`, `http.post_form`, `http.put`, `http.patch`,
`http.head` and `http.delete` cover the common verbs. `http.fetch` takes a
`FetchConfig` when you need headers, cookies, timeouts, redirect control,
client certificates or progress callbacks:

```v ignore
import net.http

resp := http.fetch(
	url:    'https://example.com/api'
	method: .post
	header: http.new_header(key: .content_type, value: 'application/json')
	data:   '{"hello":"world"}'
)!
```

HTTPS requests use HTTP/1.1 by default. Set `enable_http2: true` in
`http.fetch` or a `http.Request` to opt in to HTTP/2 when the server supports it.

A positive `stop_copying_limit` caps the stored HTTP/1.1 or HTTP/2 response body in bytes,
independently of response headers and network read boundaries. HTTP/1.1 preserves the full body
for nonpositive limits. Chunked HTTP/1.1 responses count body bytes after removing chunk framing.
HTTP/1.1 still applies `Content-Encoding` decompression, and caps the resulting body at the limit.
If the bounded compressed prefix cannot be decompressed, it retains the encoded prefix, as with
ordinary response parsing. The client still reads the full response and invokes progress callbacks,
so streaming downloads can keep a bounded preview without losing data in the callbacks.

## Serving requests

A server is a `Handler` — anything with a `handle(Request) Response` method —
plus an `http.Server` to run it:

```v ignore
import net.http

struct App {}

fn (mut a App) handle(req http.Request) http.Response {
	return http.Response{
		status_code: 200
		body:        'you asked for ${req.url}'
	}
}

fn main() {
	mut server := &http.Server{
		addr:    ':8080'
		handler: App{}
	}
	server.listen_and_serve()
}
```

Set `cert` and `cert_key` to terminate TLS (PEM strings when
`in_memory_verification` is true, otherwise file paths). Set `enable_http2` to
serve HTTP/2: on the TLS listener it advertises ALPN `h2, http/1.1`, and on the
plain listener it accepts prior-knowledge cleartext h2c. Either way a client
that does not ask for HTTP/2 keeps the ordinary HTTP/1.1 path.

## Identifying the client

`Request.remote_addr` is the address of the peer that sent the request — the
equivalent of Go's `http.Request.RemoteAddr`. `Request.remote_ip()` returns the
same thing without the port:

```v ignore
import net.http

struct App {}

fn (mut a App) handle(req http.Request) http.Response {
	return http.Response{
		status_code: 200
		body:        'you are ${req.remote_ip()} (${req.remote_addr})'
	}
}
```

| request | `remote_addr` | `remote_ip()` |
| --- | --- | --- |
| IPv4 peer | `127.0.0.1:52134` | `127.0.0.1` |
| IPv6 peer | `[::1]:52134` | `::1` |
| link-local IPv6 peer | `[fe80::1%3]:52134` | `fe80::1%3` |
| built by you, to send | `''` | `''` |

Three things are worth knowing about it:

**It is server-side only.** `http.Server` fills it in for every request it hands
to a `Handler`, on all of its paths — plain HTTP/1.1, TLS HTTP/1.1, h2c and
HTTP/2 over TLS. A `Request` you construct yourself to send with the client has
an empty `remote_addr`, and so does a request served by `veb`, which has its own
server: use `ctx.ip()` there.

**It cannot be forged.** The address is read from the accepted socket, not from
anything the client sent. The server also mirrors the bare IP into a
`Remote-Addr` request header, which predates this field; any `Remote-Addr`
header the client sent is discarded first, in any casing, so it cannot shadow
the real one. That header is best-effort — headers live in a fixed-size array,
so a request that already filled it leaves no room — while `remote_addr` is
always set. Prefer the field.

**A scoped IPv6 address keeps its zone.** A link-local peer is reported as
`[fe80::1%3]:52134`, where `3` is the numeric interface index: a link-local
address only identifies a host together with the interface it was seen on, and
without the zone the address cannot be dialled back. `net.split_address` and
`net.dial_tcp` both accept that form.

### Behind a reverse proxy

If your server sits behind nginx, a load balancer or a CDN, then the proxy is
the peer: `remote_addr` is the proxy's address, which is correct but probably
not what you want to log or rate-limit on.

The original client address is only available in a header the proxy adds —
`X-Forwarded-For`, `X-Real-Ip`, `CF-Connecting-IP` and friends. **Those are
request headers like any other: a client that can reach your server directly
can send whatever it likes in them.** They are trustworthy only if nothing but
your proxy can open a connection to the server, and only after you have
configured the proxy to overwrite (not append to) them.

So use `remote_addr` to decide whether the connection came from your proxy, and
only then read the forwarded header:

```v ignore
import net.http

const trusted_proxy = '10.0.0.1'

fn client_ip(req http.Request) string {
	if req.remote_ip() != trusted_proxy {
		return req.remote_ip()
	}
	// X-Forwarded-For is a list, oldest first; the last entry is the one our
	// own proxy appended, and the only one it vouches for.
	forwarded := req.header.get(.x_forwarded_for) or { return req.remote_ip() }
	return forwarded.all_after_last(',').trim_space()
}
```

## Cookies, headers and multipart

`Request.add_cookie`/`Request.cookie` and `Response.cookies` handle cookies.
`Response.cookies` parses `Expires` with `time.parse_http_header_string`. Invalid dates
and years before 1601 remain in `Cookie.unparsed`; they do not replace a valid expiry.
`Cookie.raw_expires` retains date text even when it cannot be parsed.
When serializing `Cookie`, IPv4 literals such as `127.0.0.1` are accepted as domains;
invalid domains are omitted.

`Header` provides both `CommonHeader` enum access (`get`, `set`, `add`) and
string access (`get_custom`, `set_custom`, `add_custom`). `http.parse_form`,
`http.parse_multipart_form` and `http.post_multipart_form` cover form bodies.

`Cookie.str()` and `sanitize_cookie_value` remove invalid bytes from cookie values,
including control characters, quotes, semicolons and backslashes. Spaces and commas
inside the value are preserved; the sanitized value is quoted when it begins or ends
with a space or comma.

### Windows TLS handshake compatibility

On Windows, HTTPS uses Schannel first. If its handshake returns `SEC_E_INVALID_TOKEN`
(`0x80090308`), the client retries the connection using the configured `net.ssl` backend
(mbedTLS by default). This happens before sending any HTTP request bytes and also applies
to pooled connections. Other Schannel errors, including certificate failures, propagate.
Errors after sending a request do not trigger this backend fallback.

The retry preserves certificate validation, configured certificates, HTTP/2 negotiation,
request headers, method, and body. `-d no_vschannel` continues to select `net.ssl` directly.
