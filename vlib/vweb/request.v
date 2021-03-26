module vweb

import io
import strings
import net.http
import net.urllib

fn parse_request(mut reader io.BufferedReader) ?http.Request {
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
	mut body := []byte{}
	if length := h.get(.content_length) {
		n := length.int()
		if n > 0 {
			body = []byte{len: n}
			reader.read(mut body) or {}
		}
	}
	h.free()

	return http.Request{
		method: method
		url: target.str()
		headers: headers
		lheaders: lheaders
		data: body.bytestr()
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
	if !s.contains(':') {
		return error('missing colon in header')
	}
	words := s.split_nth(':', 2)
	// TODO: parse quoted text according to the RFC
	return words[0], words[1].trim_left(' \t')
}

// Parse URL encoded key=value&key=value forms
fn parse_form(body string) map[string]string {
	words := body.split('&')
	mut form := map[string]string{}
	for word in words {
		kv := word.split_nth('=', 2)
		if kv.len != 2 {
			continue
		}
		key := urllib.query_unescape(kv[0]) or { continue }
		val := urllib.query_unescape(kv[1]) or { continue }
		form[key] = val
	}
	return form
	// }
	// todo: parse form-data and application/json
	// ...
}

fn parse_multipart_form(body string, boundary string) (map[string]string, map[string][]FileData) {
	sections := body.split(boundary)
	fields := sections[1..sections.len - 1]
	mut form := map[string]string{}
	mut files := map[string][]FileData{}

	for field in fields {
		// TODO: do not split into lines; do same parsing for HTTP body
		lines := field.split_into_lines()[1..]
		disposition := parse_disposition(lines[0])
		// Grab everything between the double quotes
		name := disposition['name'] or { continue }
		// Parse files
		// TODO: filename*
		if 'filename' in disposition {
			filename := disposition['filename']
			// Parse Content-Type header
			if lines.len == 1 || !lines[1].to_lower().starts_with('content-type:') {
				continue
			}
			mut ct := lines[1].split_nth(':', 2)[1]
			ct = ct.trim_left(' \t')
			data := lines_to_string(field.len, lines, 3, lines.len - 1)
			files[name] << FileData{
				filename: filename
				content_type: ct
				data: data
			}
			continue
		}
		data := lines_to_string(field.len, lines, 2, lines.len - 1)
		form[name] = data
	}
	return form, files
}

// Parse the Content-Disposition header of a multipart form
// Returns a map of the key="value" pairs
// Example: parse_disposition('Content-Disposition: form-data; name="a"; filename="b"') == {'name': 'a', 'filename': 'b'}
fn parse_disposition(line string) map[string]string {
	mut data := map[string]string{}
	for word in line.split(';') {
		kv := word.split_nth('=', 2)
		if kv.len != 2 {
			continue
		}
		key, value := kv[0].to_lower().trim_left(' \t'), kv[1]
		if value.starts_with('"') && value.ends_with('"') {
			data[key] = value[1..value.len - 1]
		} else {
			data[key] = value
		}
	}
	return data
}

[manualfree]
fn lines_to_string(len int, lines []string, start int, end int) string {
	mut sb := strings.new_builder(len)
	for i in start .. end {
		sb.writeln(lines[i])
	}
	res := sb.str()
	unsafe { sb.free() }
	return res
}
