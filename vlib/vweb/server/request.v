module server

import io
import net.http
import net.urllib

enum HttpVersion {
	v1_0
	v1_1
	v2_0
}

// HTTP request received as a server
struct Request {
	http.Request
pub:
	parsed_headers map[string][]string
	version        HttpVersion
}

pub fn parse_request(mut reader io.BufferedReader) ?Request {
	// request line
	method, target, version := parse_request_line(reader.read_line() ?) ?

	// headers
	mut headers := map[string][]string{}
	mut line := reader.read_line() ?
	for line != '' {
		key, values := parse_header(line) ?
		headers[key] << values
		headers[key.to_lower()] << values
		line = reader.read_line() ?
	}

	// body
	mut body := [byte(0)]
	if 'content-length' in headers {
		body = []byte{len: headers['content-length'][0].int()}
		reader.read(mut body) or { }
	}

	mut http_headers := map[string]string{}
	for k, v in headers {
		http_headers[k] = v.join('; ')
	}

	return Request{
		Request: {
			method: method
			url: target.str()
			headers: http_headers
			data: string(body)
		}
		parsed_headers: headers
		version: version
	}
}

fn parse_request_line(s string) ?(http.Method, urllib.URL, HttpVersion) {
	words := s.split(' ')
	if words.len != 3 {
		return error('malformed request line')
	}
	method := http.method_from_str(words[0])
	target := urllib.parse(words[1]) ?

	mut version := HttpVersion.v1_0
	if words[2] == 'HTTP/1.0' {
		version = .v1_0
	} else if words[2] == 'HTTP/1.1' {
		version = .v1_1
	} else if words[2] == 'HTTP/2.0' {
		version = .v2_0
	} else {
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
		if c !in server.token_chars {
			return false
		}
	}
	return true
}
