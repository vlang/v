module http
import net.http.chunked

// Response represents the result of the request
pub struct Response {
pub:
	text        string
	header      Header
	cookies     map[string]string
	status_code Status
}

fn (mut resp Response) free() {
	unsafe { resp.header.data.free() }
}

pub fn parse_response(resp string) Response {
	mut header := new_header()
	// TODO: Cookie data type
	mut cookies := map[string]string{}
	first_header := resp.all_before('\n')
	mut status_code := 0
	if first_header.contains('HTTP/') {
		val := first_header.find_between(' ', ' ')
		status_code = val.int()
	}
	mut text := ''
	// Build resp header map and separate the body
	mut nl_pos := 3
	mut i := 1
	for {
		old_pos := nl_pos
		nl_pos = resp.index_after('\n', nl_pos + 1)
		if nl_pos == -1 {
			break
		}
		h := resp[old_pos + 1..nl_pos]
		// End of headers
		if h.len <= 1 {
			text = resp[nl_pos + 1..]
			break
		}
		i++
		pos := h.index(':') or { continue }
		mut key := h[..pos]
		val := h[pos + 2..].trim_space()
		header.add_custom(key, val) or { eprintln('error parsing header: $err') }
	}
	// set cookies
	for cookie in header.values(.set_cookie) {
		parts := cookie.split_nth('=', 2)
		cookies[parts[0]] = parts[1]
	}
	if header.get(.transfer_encoding) or { '' } == 'chunked' || header.get(.content_length) or { '' } == '' {
		text = chunked.decode(text)
	}
	return Response{
		status_code: status_from_int(status_code)
		header: header
		cookies: cookies
		text: text
	}
}

