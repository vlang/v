module veb

import strings
import time

// AccessLogOptions configures the `access_log` middleware: one JSON line per
// response on stdout, carrying method, path, status, bytes, latency and the
// request id when one is present. Standard output is the destination on
// purpose — collectors, files and rotation belong outside the process.
@[params]
pub struct AccessLogOptions {
pub:
	// include_query decides whether the logged path keeps its query string.
	// Query strings often carry tokens and secrets; the default drops them.
	include_query bool
}

// AccessLog is the stamp half and the log half of one access log sharing
// nothing but the request context: veb middleware runs before a handler or
// after it, never around it.
pub struct AccessLog[T] {
	options AccessLogOptions
}

// access_log builds the pair. Register both halves, globally or on the same
// route:
// ```v
// log := veb.access_log[Context]()
// app.use(log.stamp())
// app.use(log.finish())
// ```
pub fn access_log[T](options AccessLogOptions) AccessLog[T] {
	return AccessLog[T]{
		options: options
	}
}

// stamp records the arrival millisecond on the request context.
pub fn (l &AccessLog[T]) stamp() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [l] [T](mut ctx T) bool {
			ctx.Context.access_log_start_ms = time.now().unix_milli()
			return true
		}
	}
}

// finish writes one JSON line for the response the handler produced, then
// lets it through. A request the stamp half never saw still logs; its
// latency is -1, which is honest about what is unknown.
pub fn (l &AccessLog[T]) finish() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		after:   true
		handler: fn [l] [T](mut ctx T) bool {
			println(format_access_log(l.options, mut ctx.Context))
			return true
		}
	}
}

// format_access_log renders one response as a JSON log line. It is a pure
// function of the finished context so the format is testable without a
// server, a clock, or stdout.
pub fn format_access_log(o AccessLogOptions, mut ctx Context) string {
	start := ctx.access_log_start_ms
	latency_ms := if start > 0 {
		time.now().unix_milli() - start
	} else {
		-1
	}
	path := if o.include_query {
		ctx.req.url
	} else {
		ctx.req.url.all_before('?')
	}
	request_id := ctx.res.header.get_custom('X-Request-ID') or { '' }
	return '{"method":"${ctx.req.method}","path":"${json_escape(path)}","status":${ctx.res.status_code},"bytes":${ctx.res.body.len},"latency_ms":${latency_ms},"request_id":"${json_escape(request_id)}"}'
}

// json_escape renders a string safe for embedding in a JSON double-quoted
// value. The quote, the backslash and control characters are escaped; the
// log line must stay one line and valid JSON whatever the path contains.
fn json_escape(s string) string {
	mut sb := strings.new_builder(s.len)
	for c in s.bytes() {
		match c {
			`"` {
				sb.write_string('\\"')
			}
			`\\` {
				sb.write_string('\\\\')
			}
			`\n` {
				sb.write_string('\\n')
			}
			`\r` {
				sb.write_string('\\r')
			}
			`\t` {
				sb.write_string('\\t')
			}
			else {
				if c < 0x20 {
					sb.write_string('\\u00')
					sb.write_string(c.hex())
				} else {
					sb.write_string(c.ascii_str())
				}
			}
		}
	}
	return sb.str()
}
