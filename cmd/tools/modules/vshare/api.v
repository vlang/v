module vshare

import json2

const max_response_excerpt_len = 200

struct ShareResponse {
	hash  string
	error string
}

// share_url_from_response validates the playground share API response and returns the public URL.
pub fn share_url_from_response(status_code int, body string) !string {
	if status_code < 200 || status_code >= 300 {
		excerpt := response_excerpt(body)
		if excerpt == '' {
			return error('Failed to share code: playground returned HTTP ${status_code}')
		}
		return error('Failed to share code: playground returned HTTP ${status_code}: ${excerpt}')
	}
	response := json2.decode[ShareResponse](body) or {
		return error('Failed to decode playground response: ${err.msg()}')
	}
	if response.error != '' {
		return error('Failed to share code: ${response.error}')
	}
	if response.hash == '' {
		return error('Failed to share code: playground response did not include a hash')
	}
	return 'https://play.vlang.io/p/${response.hash}'
}

fn response_excerpt(body string) string {
	mut text := body.trim_space()
	text = text.replace('\r', ' ').replace('\n', ' ')
	if text.len <= max_response_excerpt_len {
		return text
	}
	return text[..max_response_excerpt_len] + '...'
}
