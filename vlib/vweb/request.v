module vweb

import io
import net.http
import net.urllib

pub fn parse_request(mut reader io.BufferedReader) ?http.Request {
	// request line
	mut line := reader.read_line() ?
	method, target, version := parse_request_line(line) ?

	// headers
	mut headers := map[string][]string{}
	line = reader.read_line() ?
	for line != '' {
		key, values := parse_header(line) ?
		headers[key] << values
		line = reader.read_line() ?
	}

	mut http_headers := map[string]string{}
	mut http_lheaders := map[string]string{}
	for k, v in headers {
		values := v.join('; ')
		http_headers[k] = values
		http_lheaders[k.to_lower()] = values
	}

	// body
	mut body := [byte(0)]
	if 'content-length' in http_lheaders {
		n := http_lheaders['content-length'].int()
		body = []byte{len: n, cap: n + 1}
		reader.read(mut body) or { }
		body << 0
	}

	return http.Request{
		method: method
		url: target.str()
		headers: http_headers
		lheaders: http_lheaders
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

fn parse_header(s string) ?(string, []string) {
	if ':' !in s {
		return error('missing colon in header')
	}
	words := s.split_nth(':', 2)
	if !is_token(words[0]) {
		return error('invalid character in header name')
	}
	// TODO: parse quoted text according to the RFC
	return words[0], words[1].trim_left(' \t').split(';').map(it.trim_space())
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
