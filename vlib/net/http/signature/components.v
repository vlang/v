// Components captures the inputs the signing/verifying side reads
// when it builds the signature base. It is deliberately decoupled
// from `http.Request` / `http.Response` so the same primitives work
// for messages produced by any HTTP stack (or for offline signing).
//
// The `request_*` and `response_*` slots overlap on purpose - a
// Components value typically describes either a request or a
// response, and unused fields stay `none`.
module signature

// Components is the side-channel input for `signature_base_string`.
// Empty / `none` fields mean "not available" - the signer / verifier
// returns `MalformedMessage` if the covered-components list mentions
// a derived component whose source is `none` here.
pub struct Components {
pub mut:
	// Derived components covered by RFC 9421 §2.2.
	method         ?string
	target_uri     ?string // full request URI, used by @target-uri
	authority      ?string // host[:port] of the request
	scheme         ?string // "http" | "https"
	request_target ?string // method-line target as-on-the-wire
	path           ?string // path component of the URI
	query          ?string // query component INCLUDING the leading "?"
	// status applies to response signing/verification (@status, §2.2.9).
	status ?int
	// fields holds HTTP header field values keyed by *lowercased*
	// field name, with the slice preserving the order multiple values
	// arrive in. Values are joined with ", " on emission per
	// RFC 9421 §2.1.
	fields map[string][]string
}

// add_field is sugar for accumulating header values without juggling
// the underlying map manually. Consecutive calls preserve insertion
// order, which matters for fields that appear more than once.
pub fn (mut c Components) add_field(name string, value string) {
	lname := name.to_lower()
	if lname in c.fields {
		mut existing := c.fields[lname]
		existing << value
		c.fields[lname] = existing
	} else {
		c.fields[lname] = [value]
	}
}

// component_value returns the canonical string the component
// contributes to the signature base. Derived components start with
// '@'; everything else is treated as an HTTP field name (lowercased
// for lookup, RFC 9421 §2.1.3).
fn (c Components) component_value(name string) !string {
	if name != '' && name[0] == `@` {
		return c.derived_value(name)!
	}
	return c.field_value(name)!
}

fn (c Components) derived_value(name string) !string {
	return match name {
		'@method' {
			c.method or { return missing(name) }
		}
		'@target-uri' {
			c.target_uri or { return missing(name) }
		}
		'@authority' {
			if a := c.authority {
				normalize_authority(a, c.scheme)
			} else {
				return missing(name)
			}
		}
		'@scheme' {
			if s := c.scheme {
				s.to_lower()
			} else {
				return missing(name)
			}
		}
		'@request-target' {
			c.request_target or { return missing(name) }
		}
		'@path' {
			c.path or { return missing(name) }
		}
		'@query' {
			// RFC 9421 §2.2.7: the value MUST include the leading "?".
			// If query is empty, the value is the single character "?".
			if q := c.query {
				if q.len == 0 || q[0] != `?` {
					'?' + q
				} else {
					q
				}
			} else {
				return missing(name)
			}
		}
		'@status' {
			if s := c.status {
				s.str()
			} else {
				return missing(name)
			}
		}
		else {
			return MalformedMessage{
				reason: 'unsupported derived component "${name}"'
			}
		}
	}
}

fn (c Components) field_value(name string) !string {
	lname := name.to_lower()
	values := c.fields[lname] or { return missing(name) }
	if values.len == 0 {
		return missing(name)
	}
	if values.len == 1 {
		return trim_ows(values[0])
	}
	mut trimmed := []string{cap: values.len}
	for v in values {
		trimmed << trim_ows(v)
	}
	return trimmed.join(', ')
}

fn missing(name string) IError {
	return MalformedMessage{
		reason: 'covered component "${name}" is not present in the message'
	}
}

// normalize_authority lowercases the authority and strips the port when
// it equals the URI scheme's default (RFC 9421 §2.2.3 + RFC 9110 §4.2.3).
// Without this, peers that emit `example.com` and peers that emit
// `example.com:443` produce different signature bases for the same
// resource and fail to interoperate.
fn normalize_authority(authority string, scheme ?string) string {
	lower := authority.to_lower()
	port_colon := find_port_colon(lower) or { return lower }
	port := lower[port_colon + 1..]
	scheme_lower := if s := scheme { s.to_lower() } else { '' }
	if (scheme_lower == 'https' && port == '443') || (scheme_lower == 'http' && port == '80') {
		return lower[..port_colon]
	}
	return lower
}

// find_port_colon returns the index of the ':' that separates the port
// in an authority, or `none` if no port is present. IPv6 literals embed
// colons inside `[...]`; the port colon (if any) is the one immediately
// following the closing bracket.
fn find_port_colon(authority string) ?int {
	if authority.starts_with('[') {
		bracket := authority.index(']') or { return none }
		if bracket + 1 < authority.len && authority[bracket + 1] == `:` {
			return bracket + 1
		}
		return none
	}
	colon := authority.last_index(':') or { return none }
	return colon
}

// trim_ows removes leading/trailing OWS (RFC 7230 §3.2.3 - SP and
// HTAB). RFC 9421 §2.1 step 3 mandates this trim before signing.
fn trim_ows(s string) string {
	mut start := 0
	mut end := s.len
	for start < end && (s[start] == ` ` || s[start] == `\t`) {
		start++
	}
	for end > start && (s[end - 1] == ` ` || s[end - 1] == `\t`) {
		end--
	}
	return s[start..end]
}
