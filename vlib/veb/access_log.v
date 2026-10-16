module veb

import encoding.utf8
import strconv
import strings
import time

// AccessLogOptions configures the `access_log` middleware: one JSON line on
// stdout for each response that reaches veb's after-middleware, carrying
// method, path, status, bytes, latency and the request id when one is present.
// Standard output is the destination on purpose — collectors, files and
// rotation belong outside the process.
//
// veb runs no after-middleware for a request that matched no route (the 404
// the router answers with) or whose response a before-middleware sent by
// returning `false` (a 401 from an auth check, a CORS rejection), so those
// responses are not logged.
@[params]
pub struct AccessLogOptions {
pub:
	// include_query decides whether the logged path keeps its query string.
	// Query strings often carry tokens and secrets; the default drops them.
	include_query bool
	// request_id_header is the response header the request id is read from.
	// Keep it equal to `request_id.Config.header` when that one is changed.
	request_id_header string = 'X-Request-ID'
}

// AccessLog is the stamp half and the log half of one access log sharing
// nothing but the request context: veb middleware runs before a handler or
// after it, never around it.
pub struct AccessLog[T] {
	options AccessLogOptions
	// write receives every finished line; tests in this module capture it.
	write fn (line string) = print_access_log_line
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

// stamp records the arrival time on the request context.
pub fn (l &AccessLog[T]) stamp() MiddlewareOptions[T] {
	return MiddlewareOptions[T]{
		handler: fn [T](mut ctx T) bool {
			ctx.Context.access_log_start_ns = i64(time.sys_mono_now())
			return true
		}
	}
}

// finish writes one JSON line for the response the handler produced, then
// lets it through. A request the stamp half never saw still logs; its
// latency is -1, which is honest about what is unknown. The handler keeps its
// own copy of the options, so the pair may be built in a helper function
// that returns before the server starts.
pub fn (l &AccessLog[T]) finish() MiddlewareOptions[T] {
	options := l.options
	write := l.write
	return MiddlewareOptions[T]{
		after:   true
		handler: fn [options, write] [T](mut ctx T) bool {
			write(format_access_log(options, &ctx.Context))
			return true
		}
	}
}

fn print_access_log_line(line string) {
	println(line)
}

// format_access_log renders one response as a JSON log line. It is a pure
// function of the finished context so the format is testable without a
// server, a clock, or stdout.
pub fn format_access_log(o AccessLogOptions, ctx &Context) string {
	start := ctx.access_log_start_ns
	latency_ms := if start > 0 {
		(i64(time.sys_mono_now()) - start) / 1_000_000
	} else {
		-1
	}
	path := if o.include_query {
		ctx.req.url
	} else {
		ctx.req.url.all_before('?')
	}
	request_id := if o.request_id_header == '' {
		''
	} else {
		ctx.res.header.get_custom(o.request_id_header) or { '' }
	}
	return '{"method":"${ctx.req.method}","path":"${json_escape(path)}","status":${ctx.res.status_code},"bytes":${access_log_bytes(ctx)},"latency_ms":${latency_ms},"request_id":"${json_escape(request_id)}"}'
}

// access_log_bytes is the size of the response body. File responses
// (`ctx.file()`, static files sent in streaming mode) leave the body empty and
// carry their size only in Content-Length, so that header wins when it is set.
fn access_log_bytes(ctx &Context) i64 {
	if value := ctx.res.header.get(.content_length) {
		if n := strconv.atoi64(value) {
			if n >= 0 {
				return n
			}
		}
	}
	return ctx.res.body.len
}

// json_escape renders a string safe for embedding in a JSON double-quoted
// value. The quote, the backslash and control characters are escaped, and a
// byte that is not part of a valid UTF-8 sequence becomes U+FFFD; the log line
// must stay one line and valid JSON whatever the path contains.
@[direct_array_access]
fn json_escape(s string) string {
	mut sb := strings.new_builder(s.len)
	mut i := 0
	for i < s.len {
		c := s[i]
		if c >= 0x80 {
			_, n := utf8.decode_rune_at(s, i)
			if n == 1 {
				sb.write_string('\\ufffd')
			} else {
				for j in i .. i + n {
					sb.write_u8(s[j])
				}
			}
			i += n
			continue
		}
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
					sb.write_u8(c)
				}
			}
		}
		i++
	}
	return sb.str()
}
