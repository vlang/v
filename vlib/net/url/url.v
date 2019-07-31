// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package url parses URLs and implements query escaping.
module url

// See RFC 3986. This package generally follows RFC 3986, except where
// it deviates for compatibility reasons. When sending changes, first
// search old issues for history on decisions. Unit tests should also
// contain references to issue numbers with details.


// Error reports an error and the operation and URL that caused it.
struct Error {
	Op  string
	URL string
	Err error
}

// fn (e *Error) Unwrap() error   { return e.Err }
// fn (e *Error) Error() string   { return e.Op + ' ' + e.URL + ': ' + e.Err.Error() }
// fn (e *Error) Timeout() bool   { return oserror.IsTimeout(e.Err) }
// fn (e *Error) Temporary() bool { return oserror.IsTemporary(e.Err) }

enum EncodingMode {
	EncodePath
	EncodePathSegment
	EncodeHost
	EncodeZone
	EncodeUserPassword
	EncodeQueryComponent
	EncodeFragment
}

const (
	InvalidHostError   = 'invalid character in host name'
	ParseParse         = 'error parsing url'
	EscapeError        = 'invalid URL escape'
	MissingSchemeError = 'missing protocol scheme'
)

fn get_error(message, p string) error {
	return error('net.url: $message ' + quote(p))
}
fn quote(p) string {
	return p
}

// type EscapeError error

// fn escape_error(e) error {
// 	return error('invalid URL escape ' + strconv.Quote(string(e))
// }

// type InvalidHostError string

// fn (e InvalidHostError) Error() string {
// 	return 'invalid character ' + strconv.Quote(string(e)) + ' in host name'
// }

// Return true if the specified character should be escaped when
// appearing in a URL string, according to RFC 3986.
//
// Please be informed that for now should_escape does not check all
// reserved characters correctly. See golang.org/issue/5684.
fn should_escape(c byte, mode EncodingMode) bool {
	// §2.3 Unreserved characters (alphanum)
	if `a` <= c && c <= `z` || `A` <= c && c <= `Z` || `0` <= c && c <= `9` {
		return false
	}

	if mode == EncodingMode.EncodeHost || mode == EncodingMode.EncodeZone {
		// §3.2.2 host allows
		//	sub-delims = `!` / `$` / `&` / ``` / `(` / `)` / `*` / `+` / `,` / `;` / `=`
		// as part of reg-name.
		// We add : because we include :port as part of host.
		// We add [ ] because we include [ipv6]:port as part of host.
		// We add < > because they`re the only characters left that
		// we could possibly allow, and parse will reject them if we
		// escape them (because hosts can`t use %-encoding for
		// ASCII bytes).
		switch c {
		case `!`, `$`, `&`, `\\`, `(`, `)`, `*`, `+`, `,`, `;`, `=`, `:`, `[`, `]`, `<`, `>`, `"`:
			return false
		}
	}

	switch c {
	case `-`, `_`, `.`, `~`: // §2.3 Unreserved characters (mark)
		return false

	case `$`, `&`, `+`, `,`, `/`, `:`, `;`, `=`, `?`, `@`: // §2.2 Reserved characters (reserved)
		// Different sections of the URL allow a few of
		// the reserved characters to appear unescaped.
		switch mode {
		case EncodingMode.EncodePath: // §3.3
			// The RFC allows : @ & = + $ but saves / ; , for assigning
			// meaning to individual path segments. This package
			// only manipulates the path as a whole, so we allow those
			// last three as well. That leaves only ? to escape.
			return c == `?`

		case EncodingMode.EncodePathSegment: // §3.3
			// The RFC allows : @ & = + $ but saves / ; , for assigning
			// meaning to individual path segments.
			return c == `/` || c == `;` || c == `,` || c == `?`

		case EncodingMode.EncodeUserPassword: // §3.2.1
			// The RFC allows `;`, `:`, `&`, `=`, `+`, `$`, and `,` in
			// userinfo, so we must escape only `@`, `/`, and `?`.
			// The parsing of userinfo treats `:` as special so we must escape
			// that too.
			return c == `@` || c == `/` || c == `?` || c == `:`

		case EncodingMode.EncodeQueryComponent: // §3.4
			// The RFC reserves (so we must escape) everything.
			return true

		case EncodingMode.EncodeFragment: // §4.1
			// The RFC text is silent but the grammar allows
			// everything, so escape nothing.
			return false
		}
	}

	if mode == EncodingMode.EncodeFragment {
		// RFC 3986 §2.2 allows not escaping sub-delims. A subset of sub-delims are
		// included in reserved from RFC 2396 §2.2. The remaining sub-delims do not
		// need to be escaped. To minimize potential breakage, we apply two restrictions:
		// (1) we always escape sub-delims outside of the fragment, and (2) we always
		// escape single quote to avoid breaking callers that had previously assumed that
		// single quotes would be escaped. See issue #19917.
		switch c {
		case `!`, `(`, `)`, `*`:
			return false
		}
	}

	// Everything else must be escaped.
	return true
}

// query_unescape does the inverse transformation of query_escape,
// converting each 3-byte encoded substring of the form '%AB' into the
// hex-decoded byte 0xAB.
// It returns an error if any % is not followed by two hexadecimal
// digits.
pub fn query_unescape(s string) ?error {
	return unescape(s, EncodingMode.EncodeQueryComponent)
}

// path_unescape does the inverse transformation of path_escape,
// converting each 3-byte encoded substring of the form '%AB' into the
// hex-decoded byte 0xAB. It returns an error if any % is not followed
// by two hexadecimal digits.
//
// path_unescape is identical to query_unescape except that it does not
// unescape '+' to ' ' (space).
pub fn path_unescape(s string) ?string {
	return unescape(s, EncodingMode.EncodePathSegment)
}

// unescape unescapes a string; the mode specifies
// which section of the URL string is being unescaped.
pub fn unescape(s string, mode encoding) ?string {
	// Count %, check that they're well-formed.
	n := 0
	hasPlus := false
	for i := 0; i < s.len; {
		switch s[i] {
		case '%':
			n++
			if i+2 >= s.len || !ishex(s[i+1]) || !ishex(s[i+2]) {
				s = s[i:]
				if s.len > 3 {
					s = s[:3]
				}
				// return '', EscapeError(s)
				return error('$EscapeError: ' + s)
			}
			// Per https://tools.ietf.org/html/rfc3986#page-21
			// in the host component %-encoding can only be used
			// for non-ASCII bytes.
			// But https://tools.ietf.org/html/rfc6874#section-2
			// introduces %25 being allowed to escape a percent sign
			// in IPv6 scoped-address literals. Yay.
			if mode == EncodingMode.EncodeHost && unhex(s[i+1]) < 8 && s.slice(i, i+3) != '%25' {
				// return '', EscapeError(s[i : i+3])
				return error('$EscapeError: ' + s)
			}
			if mode == EncodingMode.EncodeZone {
				// RFC 6874 says basically 'anything goes' for zone identifiers
				// and that even non-ASCII can be redundantly escaped,
				// but it seems prudent to restrict %-escaped bytes here to those
				// that are valid host name bytes in their unescaped form.
				// That is, you can use escaping in the zone identifier but not
				// to introduce bytes you couldn't just write directly.
				// But Windows puts spaces here! Yay.
				v := unhex(s[i+1])<<4 | unhex(s[i+2])
				if s[i:i+3] != '%25' && v != ' ' && should_escape(v, EncodingMode.EncodeHost) {
					// return '', EscapeError(s[i : i+3])
					return error('$EscapeError: ' + s.slice(i, i+3))
				}
			}
			i += 3
		case '+':
			hasPlus = mode == EncodingMode.EncodeQueryComponent
			i++
		default:
			if (mode == EncodingMode.EncodeHost || mode == EncodingMode.EncodeZone) && s[i] < 0x80 && should_escape(s[i], mode) {
				// return '', InvalidHostError(s[i : i+1])
				return error('$InvalidHostError: ' + s.slice(i, i+1))
			}
			i++
		}
	}

	if n == 0 && !hasPlus {
		return s
	}

	var t strings.Builder
	t.Grow(s.len - 2*n)
	for i := 0; i < s.len; i++ {
		switch s[i] {
		case '%':
			t.WriteByte(unhex(s[i+1])<<4 | unhex(s[i+2]))
			i += 2
		case '+':
			if mode == EncodingMode.EncodeQueryComponent {
				t.WriteByte(' ')
			} else {
				t.WriteByte('+')
			}
		default:
			t.WriteByte(s[i])
		}
	}
	return t.string()l
}

// query_escape escapes the string so it can be safely placed
// inside a URL query.
fn query_escape(s string) string {
	return escape(s, EncodingMode.EncodeQueryComponent)
}

// path_escape escapes the string so it can be safely placed inside a URL path segment,
// replacing special characters (including /) with %XX sequences as needed.
fn path_escape(s string) string {
	return escape(s, EncodingMode.EncodePathSegment)
}

fn escape(s string, mode encoding) string {
	space_count := 0
	mut hex_count := 0
	for i := 0; i < s.len; i++ {
		c := s[i]
		if should_escape(c, mode) {
			if c == ' ' && mode == EncodingMode.EncodeQueryComponent {
				space_count++
			} else {
				hex_count++
			}
		}
	}

	if space_count == 0 && hex_count == 0 {
		return s
	}

	var buf [64]byte
	var t []byte

	required := s.len + 2*hex_count
	if required <= buf.len {
		t = buf[:required]
	} else {
		t = make([]byte, required)
	}

	if hex_count == 0 {
		copy(t, s)
		for i := 0; i < s.len; i++ {
			if s[i] == ' ' {
				t[i] = '+'
			}
		}
		return string(t)
	}

	j := 0
	for i := 0; i < s.len; i++ {
		switch c := s[i]; {
		case c == ' ' && mode == EncodingMode.EncodeQueryComponent:
			t[j] = '+'
			j++
		case should_escape(c, mode):
			t[j] = '%'
			t[j+1] = '0123456789ABCDEF'[c>>4]
			t[j+2] = '0123456789ABCDEF'[c&15]
			j += 3
		default:
			t[j] = s[i]
			j++
		}
	}
	return string(t)
}

// A URL represents a parsed URL (technically, a URI reference).
//
// The general form represented is:
//
//	[scheme:][//[userinfo@]host][/]path[?query][#fragment]
//
// URLs that do not start with a slash after the scheme are interpreted as:
//
//	scheme:opaque[?query][#fragment]
//
// Note that the path field is stored in decoded form: /%47%6f%2f becomes /Go/.
// A consequence is that it is impossible to tell which slashes in the path were
// slashes in the raw URL and which were %2f. This distinction is rarely important,
// but when it is, the code should use raw_path, an optional field which only gets
// set if the default encoding is different from path.
//
// URL's String method uses the escaped_path method to obtain the path. See the
// escaped_path method for more details.
struct URL {
	scheme      string
	opaque      string    // encoded opaque data
	user        *Userinfo // username and password information
	host        string    // host or host:port
	path        string    // path (relative paths may omit leading slash)
	raw_path    string    // encoded path hint (see escaped_path method)
	force_query bool      // append a query ('?') even if raw_query is empty
	raw_query   string    // encoded query values, without '?'
	fragment    string    // fragment for references, without '#'
}

// user returns a Userinfo containing the provided username
// and no password set.
fn user(username string) *Userinfo {
	return &Userinfo{username, '', false}
}

// user_password returns a Userinfo containing the provided username
// and password.
//
// This functionality should only be used with legacy web sites.
// RFC 2396 warns that interpreting Userinfo this way
// ``is NOT RECOMMENDED, because the passing of authentication
// information in clear text (such as URI) has proven to be a
// security risk in almost every case where it has been used.''
fn user_password(username, password string) *Userinfo {
	return &Userinfo{username, password, true}
}

// The Userinfo type is an immutable encapsulation of username and
// password details for a URL. An existing Userinfo value is guaranteed
// to have a username set (potentially empty, as allowed by RFC 2396),
// and optionally a password.
struct Userinfo {
	username     string
	password     string
	password_set bool
}

// username returns the username.
fn (u &Userinfo) username() string {
	// if u == nil {
	// 	return ''
	// }
	return u.username
}

// password returns the password in case it is set, and whether it is set.
fn (u &Userinfo) password() string {
	// if u == nil {
	// 	return '', false
	// }
	// return u.password, u.password_set
	return u.password
}

// string returns the encoded userinfo information in the standard form
// of 'username[:password]'.
fn (u &Userinfo) string() string {
	// if u == nil {
	// 	return ''
	// }
	s := escape(u.username, EncodingMode.EncodeUserPassword)
	if u.password_set {
		s += ':' + escape(u.password, EncodingMode.EncodeUserPassword)
	}
	return s
}

// Maybe rawurl is of the form scheme:path.
// (scheme must be [a-zA-Z][a-zA-Z0-9+-.]*)
// If so, return scheme, path; else return '', rawurl.
fn split_by_scheme(rawurl string) ?[]string {
	for i := 0; i < rawurl.len; i++ {
		c := rawurl[i]
		switch {
		case 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z':
		// do nothing
		case '0' <= c && c <= '9' || c == '+' || c == '-' || c == '.':
			if i == 0 {
				return ['', rawurl]
			}
		case c == ':':
			if i == 0 {
				return errors('$MissingSchemeError')
			}
			return rawurl.left(i), rawurl.right(i+1)
		default:
			// we have encountered an invalid character,
			// so there is no valid scheme
			return ['', rawurl]
		}
	}
	return ['', rawurl]
}

fn get_scheme(rawurl string) ?string {
	return split_scheme(rawurl)[0]
}

// Maybe s is of the form t c u.
// If so, return t, c u (or t, u if cutc == true).
// If not, return s, ''.
fn split(s string, c string, cutc bool) []string {
	i := s.index(c)
	if i < 0 {
		return [s, '']
	}
	if cutc {
		return [s.left(i), s.right(i+c.len)]
	}
	return [s.left(i), s.right(i:)]
}

// parse parses rawurl into a URL structure.
//
// The rawurl may be relative (a path, without a host) or absolute
// (starting with a scheme). Trying to parse a hostname and path
// without a scheme is invalid but may not necessarily return an
// error, due to parsing ambiguities.
fn parse(rawurl string) ?URL {
	// Cut off #frag
	p := split(rawurl, '#', true)
	u := p[0]
	frag := p[1]
	url, err := parse_request_url(u, false)
	if err != nil {
		return nil, &Error{'parse', u, err}
	}
	if frag == '' {
		return url, nil
	}
	if url.fragment, err = unescape(frag, EncodingMode.EncodeFragment); err != nil {
		return nil, &Error{'parse', rawurl, err}
	}
	return url, nil
}

// parse_request_uri parses rawurl into a URL structure. It assumes that
// rawurl was received in an HTTP request, so the rawurl is interpreted
// only as an absolute URI or an absolute path.
// The string rawurl is assumed not to have a #fragment suffix.
// (Web browsers strip #fragment before sending the URL to a web server.)
fn parse_request_uri(rawurl string) ?URL {
	url := parse_request_url(rawurl, true) or {
		return err
	}
	return url
}

// parse parses a URL from a string in one of two contexts. If
// via_request is true, the URL is assumed to have arrived via an HTTP request,
// in which case only absolute URLs or path-absolute relative URLs are allowed.
// If via_request is false, all forms of relative URLs are allowed.
fn parse_request_url(rawurl string, via_request bool) ?URL {
	if string_contains_ctl_byte(rawurl) {
		return nil, errors.New('net/url: invalid control character in URL')
	}

	if rawurl == '' && via_request {
		return nil, errors.New('empty url')
	}
	url := new(URL)

	if rawurl == '*' {
		url.path = '*'
		return url, nil
	}

	// Split off possible leading 'http:', 'mailto:', etc.
	// Cannot contain escaped characters.
	p := split_by_scheme(rawurl) or {
		return err
	}
	url.scheme = p[0]
	rest := p[1]
	url.scheme = url.scheme.to_lower()

	if rest.ends_with('?') && strings.Count(rest, '?') == 1 {
		url.force_query = true
		rest = rest[:rest.len-1]
	} else {
		rest, url.raw_query = split(rest, '?', true)
	}

	if !rest.starts_with('/') {
		if url.scheme != '' {
			// We consider rootless paths per RFC 3986 as opaque.
			url.opaque = rest
			return url, nil
		}
		if via_request {
			return nil, errors.New('invalid URI for request')
		}

		// Avoid confusion with malformed schemes, like cache_object:foo/bar.
		// See golang.org/issue/16822.
		//
		// RFC 3986, §3.3:
		// In addition, a URI reference (Section 4.1) may be a relative-path reference,
		// in which case the first path segment cannot contain a colon (':') character.
		colon := rest.index(':')
		slash := rest.index('/')
		if colon >= 0 && (slash < 0 || colon < slash) {
			// First path segment has colon. Not allowed in relative URL.
			return nil, errors.New('first path segment in URL cannot contain colon')
		}
	}

	if (url.scheme != '' || !via_request && !rest.starts_with('///')) && rest.starts_with('//') {
		var authority string
		parts := split(rest.right(2), '/', false)
		authority := parts[0]
		rest := parts[1]
		url.user, url.host, err = parse_authority(authority)
		if err != nil {
			return nil, err
		}
	}
	// Set path and, optionally, raw_path.
	// raw_path is a hint of the encoding of path. We don't want to set it if
	// the default escaping of path is equivalent, to help make sure that people
	// don't rely on it in general.
	if err := url.set_path(rest); err != nil {
		return nil, err
	}
	return url, nil
}

struct ParseAuthorityRes {
	user Userinfo
	host string
}

fn parse_authority(authority string) ?ParseAuthorityRes {
	i := authority.last_index('@')
	host := ''
	user := Userinfo{}
	if i < 0 {
		parse_host(authority) or {
			return err
		}
	} else {
		host = parse_host(authority.left(i+1)) or {
			return err
		}
	}
	if i < 0 {
		return ParseAuthorityRes{host: host}
	}
	userinfo := authority.left(i)
	if !valid_userinfo(userinfo) {
		return error('net/url: invalid userinfo')
	}
	if !userinfo.contains(':') {
		if userinfo := unescape(userinfo, EncodingMode.EncodeUserPassword) or {
			return err
		}
		user = user(userinfo)
	} else {
		parts := split(userinfo, ':', true)
		username := parts[0]
		password := parts[1]
		username := unescape(username, EncodingMode.EncodeUserPassword) or {
			return err
		}
		password := unescape(password, EncodingMode.EncodeUserPassword) or {
			return err
		}
		user = user_password(username, password)
	}
	return ParseAuthorityRes{
		user: user
		host: host
	}
}

// parse_host parses host as an authority without user
// information. That is, as host[:port].
fn parse_host(host string) ?string {
	if hoststarts_with('[') {
		// parse an IP-Literal in RFC 3986 and RFC 6874.
		// E.g., '[fe80::1]', '[fe80::1%25en0]', '[fe80::1]:80'.
		i := strings.LastIndex(host, ']')
		if i < 0 {
			return '', errors.New('missing ']' in host')
		}
		colonport := host[i+1:]
		if !valid_optional_port(colonport) {
			return '', fmt.Errorf('invalid port %q after host', colonport)
		}

		// RFC 6874 defines that %25 (%-encoded percent) introduces
		// the zone identifier, and the zone identifier can use basically
		// any %-encoding it likes. That's different from the host, which
		// can only %-encode non-ASCII bytes.
		// We do impose some restrictions on the zone, to avoid stupidity
		// like newlines.
		zone := host.left(i).index('%25')
		if zone >= 0 {
			host1, err := unescape(host.left(zone), EncodingMode.EncodeHost)
			if err != nil {
				return '', err
			}
			host2, err := unescape(host.slice(zone, i), EncodingMode.EncodeZone)
			if err != nil {
				return '', err
			}
			host3, err := unescape(host.right(i), EncodingMode.EncodeHost)
			if err != nil {
				return '', err
			}
			return host1 + host2 + host3, nil
		}
	}

	var err error
	if host, err = unescape(host, EncodingMode.EncodeHost); err != nil {
		return '', err
	}
	return host, nil
}

// set_path sets the path and raw_path fields of the URL based on the provided
// escaped path p. It maintains the invariant that raw_path is only specified
// when it differs from the default encoding of the path.
// For example:
// - set_path('/foo/bar')   will set path='/foo/bar' and raw_path=''
// - set_path('/foo%2fbar') will set path='/foo/bar' and raw_path='/foo%2fbar'
// set_path will return an error only if the provided path contains an invalid
// escaping.
fn (u &URL) set_path(p string) error {
	path, err := unescape(p, EncodingMode.EncodePath)
	if err != nil {
		return err
	}
	u.path = path
	if escp := escape(path, EncodingMode.EncodePath); p == escp {
		// Default encoding is fine.
		u.raw_path = ''
	} else {
		u.raw_path = p
	}
	return nil
}

// escaped_path returns the escaped form of u.path.
// In general there are multiple possible escaped forms of any path.
// escaped_path returns u.raw_path when it is a valid escaping of u.path.
// Otherwise escaped_path ignores u.raw_path and computes an escaped
// form on its own.
// The String and request_uri methods use escaped_path to construct
// their results.
// In general, code should call escaped_path instead of
// reading u.raw_path directly.
fn (u &URL) escaped_path() string {
	if u.raw_path != '' && valid_encoded_path(u.raw_path) {
		p, err := unescape(u.raw_path, EncodingMode.EncodePath)
		if err == nil && p == u.path {
			return u.raw_path
		}
	}
	if u.path == '*' {
		return '*' // don't escape (Issue 11202)
	}
	return escape(u.path, EncodingMode.EncodePath)
}

// valid_encoded_path reports whether s is a valid encoded path.
// It must not contain any bytes that require escaping during path encoding.
fn valid_encoded_path(s string) bool {
	for i := 0; i < s.len; i++ {
		// RFC 3986, Appendix A.
		// pchar = unreserved / pct-encoded / sub-delims / ':' / '@'.
		// should_escape is not quite compliant with the RFC,
		// so we check the sub-delims ourselves and let
		// should_escape handle the others.
		switch s[i] {
		case '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=', ':', '@':
			// ok
		case '[', ']':
			// ok - not specified in RFC 3986 but left alone by modern browsers
		case '%':
			// ok - percent encoded, will decode
		default:
			if should_escape(s[i], EncodingMode.EncodePath) {
				return false
			}
		}
	}
	return true
}

// valid_optional_port reports whether port is either an empty string
// or matches /^:\d*$/
fn valid_optional_port(port string) bool {
	if port == '' {
		return true
	}
	if port[0] != ':' {
		return false
	}
	for _, b := range port[1:] {
		if b < '0' || b > '9' {
			return false
		}
	}
	return true
}

// String reassembles the URL into a valid URL string.
// The general form of the result is one of:
//
//	scheme:opaque?query#fragment
//	scheme://userinfo@host/path?query#fragment
//
// If u.opaque is non-empty, String uses the first form;
// otherwise it uses the second form.
// Any non-ASCII characters in host are escaped.
// To obtain the path, String uses u.escaped_path().
//
// In the second form, the following rules apply:
//	- if u.scheme is empty, scheme: is omitted.
//	- if u.user is nil, userinfo@ is omitted.
//	- if u.host is empty, host/ is omitted.
//	- if u.scheme and u.host are empty and u.user is nil,
//	   the entire scheme://userinfo@host/ is omitted.
//	- if u.host is non-empty and u.path begins with a /,
//	   the form host/path does not add its own /.
//	- if u.raw_query is empty, ?query is omitted.
//	- if u.fragment is empty, #fragment is omitted.
fn (u &URL) string() string {
	var buf strings.Builder
	if u.scheme != '' {
		buf.WriteString(u.scheme)
		buf.WriteByte(':')
	}
	if u.opaque != '' {
		buf.WriteString(u.opaque)
	} else {
		if u.scheme != '' || u.host != '' || u.user != nil {
			if u.host != '' || u.path != '' || u.user != nil {
				buf.WriteString('//')
			}
			if ui := u.user; ui != nil {
				buf.WriteString(ui.string())
				buf.WriteByte('@')
			}
			if h := u.host; h != '' {
				buf.WriteString(escape(h, EncodingMode.EncodeHost))
			}
		}
		path := u.escaped_path()
		if path != '' && path[0] != '/' && u.host != '' {
			buf.WriteByte('/')
		}
		if buf.Len() == 0 {
			// RFC 3986 §4.2
			// A path segment that contains a colon character (e.g., 'this:that')
			// cannot be used as the first segment of a relative-path reference, as
			// it would be mistaken for a scheme name. Such a segment must be
			// preceded by a dot-segment (e.g., './this:that') to make a relative-
			// path reference.
			if i := strings.IndexByte(path, ':'); i > -1 && strings.IndexByte(path[:i], '/') == -1 {
				buf.WriteString('./')
			}
		}
		buf.WriteString(path)
	}
	if u.force_query || u.raw_query != '' {
		buf.WriteByte('?')
		buf.WriteString(u.raw_query)
	}
	if u.fragment != '' {
		buf.WriteByte('#')
		buf.WriteString(escape(u.fragment, EncodingMode.EncodeFragment))
	}
	return buf.string()
}

// Values maps a string key to a list of values.
// It is typically used for query parameters and form values.
// Unlike in the http.Header map, the keys in a Values map
// are case-sensitive.
struct Value {
	data []string
}
type Values map[string]Value

// Get gets the first value associated with the given key.
// If there are no values associated with the key, Get returns
// the empty string. To access multiple values, use the map
// directly.
fn (v Values) get(key string) string {
	if v == nil {
		return ''
	}
	vs := v[key]
	if vs.data.len == 0 {
		return ''
	}
	return vs.data[0]
}

// Set sets the key to value. It replaces any existing
// values.
fn (v Values) set(key, value string) {
	v[key].data = [value]
}

// Add adds the value to key. It appends to any existing
// values associated with key.
fn (v Values) add(key, value string) {
	v[key].data << value
}

// Del deletes the values associated with key.
fn (v Values) del(key string) {
	v.delete(key)
}

// parseQuery parses the URL-encoded query string and returns
// a map listing the values specified for each key.
// parseQuery always returns a non-nil map containing all the
// valid query parameters found; err describes the first decoding error
// encountered, if any.
//
// Query is expected to be a list of key=value settings separated by
// ampersands or semicolons. A setting without an equals sign is
// interpreted as a key set to an empty value.
fn parse_query(query string) ?Values {
	m := Values{}
	m := _parse_query(m, query) or {
		return error(err)
	}
	return m
}

fn _parse_query(m Values, query string) ?bool {
	for query != '' {
		key := query
		if i := key.index_any('&;'); i >= 0 {
			key, query = key[:i], key[i+1:]
		} else {
			query = ''
		}
		if key == '' {
			continue
		}
		value := ''
		if i := key.index('='); i >= 0 {
			key, value = key[:i], key[i+1:]
		}
		key, err1 := query_unescape(key)
		if err1 != nil {
			if err == nil {
				err = err1
			}
			continue
		}
		value, err1 = query_unescape(value)
		if err1 != nil {
			if err == nil {
				err = err1
			}
			continue
		}
		m[key].data << value
	}
	return err
}

// encode encodes the values into ``URL encoded'' form
// ('bar=baz&foo=quux') sorted by key.
fn (v Values) encode() string {
	if v.size() == 0 {
		return ''
	}
	mut buf := strings.new_builder(1000) 
	keys := []string
	for k := range v {
		keys = << k
	}
	keys.sort()
	for _, k := range keys {
		vs := v[k]
		keyEscaped := query_escape(k)
		for _, v := range vs {
			if buf.Len() > 0 {
				buf.WriteByte('&')
			}
			buf.WriteString(keyEscaped)
			buf.WriteByte('=')
			buf.WriteString(query_escape(v))
		}
	}
	return buf.string()
}

// resolve_path applies special path segments from refs and applies
// them to base, per RFC 3986.
fn resolve_path(base, ref string) string {
	var full string
	if ref == '' {
		full = base
	} else if ref[0] != '/' {
		i := base.last_index('/')
		full = base[:i+1] + ref
	} else {
		full = ref
	}
	if full == '' {
		return ''
	}
	var dst []string
	src := full.split('/')
	for _, elem := range src {
		switch elem {
		case '.':
			// drop
		case '..':
			if dst.len > 0 {
				dst = dst[:dst.len-1]
			}
		default:
			dst = append(dst, elem)
		}
	}
	if last := src[src.len-1]; last == '.' || last == '..' {
		// Add final slash to the joined path.
		dst = append(dst, '')
	}
	return '/' + dst.join('/').trim_left('/')
}

// is_abs reports whether the URL is absolute.
// Absolute means that it has a non-empty scheme.
fn (u &URL) is_abs() bool {
	return u.scheme != ''
}

// parse parses a URL in the context of the receiver. The provided URL
// may be relative or absolute. parse returns nil, err on parse
// failure, otherwise its return value is the same as resolve_reference.
fn (u &URL) parse(ref string) (*URL, error) {
	refurl, err := parse(ref)
	if err != nil {
		return nil, err
	}
	return u.resolve_reference(refurl), nil
}

// resolve_reference resolves a URI reference to an absolute URI from
// an absolute base URI u, per RFC 3986 Section 5.2. The URI reference
// may be relative or absolute. resolve_reference always returns a new
// URL instance, even if the returned URL is identical to either the
// base or reference. If ref is an absolute URL, then resolve_reference
// ignores base and returns a copy of ref.
fn (u &URL) resolve_reference(ref &URL) *URL {
	url := *ref
	if ref.scheme == '' {
		url.scheme = u.scheme
	}
	if ref.scheme != '' || ref.host != '' || ref.user != nil {
		// The 'absoluteURI' or 'net_path' cases.
		// We can ignore the error from set_path since we know we provided a
		// validly-escaped path.
		url.set_path(resolve_path(ref.escaped_path(), ''))
		return &url
	}
	if ref.opaque != '' {
		url.user = nil
		url.host = ''
		url.path = ''
		return &url
	}
	if ref.path == '' && ref.raw_query == '' {
		url.raw_query = u.raw_query
		if ref.fragment == '' {
			url.fragment = u.fragment
		}
	}
	// The 'abs_path' or 'rel_path' cases.
	url.host = u.host
	url.user = u.user
	url.set_path(resolve_path(u.escaped_path(), ref.escaped_path()))
	return &url
}

// query parses raw_query and returns the corresponding values.
// It silently discards malformed value pairs.
// To check errors use parseQuery.
fn (u &URL) query() Values {
	v, _ := parsequery(u.raw_query)
	return v
}

// request_uri returns the encoded path?query or opaque?query
// string that would be used in an HTTP request for u.
fn (u &URL) request_uri() string {
	result := u.opaque
	if result == '' {
		result = u.escaped_path()
		if result == '' {
			result = '/'
		}
	} else {
		if result.starts_with('//') {
			result = u.scheme + ':' + result
		}
	}
	if u.force_query || u.raw_query != '' {
		result += '?' + u.raw_query
	}
	return result
}

// hostname returns u.host, without any port number.
//
// If host is an IPv6 literal with a port number, hostname returns the
// IPv6 literal without the square brackets. IPv6 literals may include
// a zone identifier.
pub fn (u &URL) hostname() string {
	return strip_port(u.host)
}

// port returns the port part of u.host, without the leading colon.
// If u.host doesn't contain a port, port returns an empty string.
pub fn (u &URL) port() string {
	return port_only(u.host)
}

fn strip_port(hostport string) string {
	// colon := strings.IndexByte(hostport, ':')
	colon := hostport.index(':')
	if colon == -1 {
		return hostport
	}
	// if i := strings.IndexByte(hostport, ']'); i != -1 {
	if i := hostport.index(']'); i != -1 {
		return hostportz.left(i).trim_left('[')
	}
	return hostport.left(colon)
}

fn port_only(hostport string) string {
	// colon := strings.IndexByte(hostport, ':')
	colon := hostport.index(':')
	if colon == -1 {
		return ''
	}
	if i := hostport.index(']:'); i != -1 {
		return hostport.right(i+']:'.len)
	}
	if hostport.contains(']') {
		return ''
	}
	return hostport.right(colon+':'.len)
}

// Marshaling interface implementations.
// Would like to implement MarshalText/UnmarshalText but that will change the JSON representation of URLs.

// fn (u &URL) MarshalBinary() (text []byte, err error) {
// 	return []byte(u.string()), nil
// }

// fn (u &URL) UnmarshalBinary(text []byte) error {
// 	u1, err := parse(string(text))
// 	if err != nil {
// 		return err
// 	}
// 	*u = *u1
// 	return nil
// }

// valid_userinfo reports whether s is a valid userinfo string per RFC 3986
// Section 3.2.1:
//     userinfo    = *( unreserved / pct-encoded / sub-delims / ':' )
//     unreserved  = ALPHA / DIGIT / '-' / '.' / '_' / '~'
//     sub-delims  = '!' / '$' / '&' / ''' / '(' / ')'
//                   / '*' / '+' / ',' / ';' / '='
//
// It doesn't validate pct-encoded. The caller does that via fn unescape.
fn valid_userinfo(s string) bool {
	for _, r := range s {
		if 'A' <= r && r <= 'Z' {
			continue
		}
		if 'a' <= r && r <= 'z' {
			continue
		}
		if '0' <= r && r <= '9' {
			continue
		}
		switch r {
		case '-', '.', '_', ':', '~', '!', '$', '&', '\'',
			'(', ')', '*', '+', ',', ';', '=', '%', '@':
			continue
		default:
			return false
		}
	}
	return true
}

// string_contains_ctl_byte reports whether s contains any ASCII control character.
fn string_contains_ctl_byte(s string) bool {
	for i := 0; i < s.len; i++ {
		b := s[i]
		if b < ' ' || b == 0x7f {
			return true
		}
	}
	return false
}

fn ishex(c byte) bool {
	switch {
	case '0' <= c && c <= '9':
		return true
	case 'a' <= c && c <= 'f':
		return true
	case 'A' <= c && c <= 'F':
		return true
	}
	return false
}

fn unhex(c byte) byte {
	switch {
	case '0' <= c && c <= '9':
		return c - '0'
	case 'a' <= c && c <= 'f':
		return c - 'a' + 10
	case 'A' <= c && c <= 'F':
		return c - 'A' + 10
	}
	return 0
}

