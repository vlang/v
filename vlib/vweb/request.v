module vweb

import io
import net.http
import net.urllib

pub fn parse_request(mut reader io.BufferedReader) ?http.Request {
	// request line
	mut line := reader.read_line() ?
	method, target, version := parse_request_line(line) ?

	// headers
	mut h := http.new_header()
	line = reader.read_line() ?
	for line != '' {
		key, value := parse_header(line) ?
		h.add_str(key, value) ?
		line = reader.read_line() ?
	}

	// create map[string]string from headers
	// TODO: replace headers and lheaders with http.Header type
	mut headers := map[string]string{}
	mut lheaders := map[string]string{}
	for key in h.keys() {
		values := h.values_str(key).join('; ')
		headers[key] = values
		lheaders[key.to_lower()] = values
	}

	// body
	mut body := [byte(0)]
	if length := h.get(.content_length) {
		n := length.int()
		body = []byte{len: n, cap: n + 1}
		reader.read(mut body) or { }
		body << 0
	}

	return http.Request{
		method: method
		url: target.str()
		headers: headers
		lheaders: lheaders
		data: string(body)
		version: version
	}
}

fn parse_request_line(s string) ?(http.Method, urllib.URL, http.Version) {
	words := s.split(' ')
	if words.len != 3 {
		return error('malformed request line')
	}
	method := http.method_from_str(words[0])
	target := urllib.parse(words[1]) ?
	version := http.version_from_str(words[2])
	if version == .unknown {
		return error('unsupported version')
	}

	return method, target, version
}

fn parse_header(s string) ?(string, string) {
	if ':' !in s {
		return error('missing colon in header')
	}
	words := s.split_nth(':', 2)
	if !is_token(words[0]) {
		return error('invalid character in header name')
	}
	// TODO: parse quoted text according to the RFC
	return words[0], words[1].trim_left(' \t')
}

// TODO: use map for faster lookup (untested)
const token_chars = r"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&'*+-.^_`|~".bytes()

fn is_token(s string) bool {
	for c in s {
		if c !in vweb.token_chars {
			return false
		}
	}
	return true
}
