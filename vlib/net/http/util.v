// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net

const vschannel_connect_failed_msg = 'Failed to connect to host'

const vschannel_sec_e_internal_error = -2146893052

// A fast version that avoids allocations in s.split()
// returns the locations of the 2 spaces
// "GET / HTTP/1.1" => ["GET" "/" "HTTP/1.1"]
fn fast_request_words(line string) (int, int) {
	space1 := line.index(' ') or { return 0, 0 }
	space2 := line.index_after(' ', space1 + 1) or { return 0, 0 }
	return space1, space2
}

fn vschannel_error_message(err_code int) string {
	$if windows {
		if err_code >= int(net.WsaError.wsaeintr)
			&& err_code <= int(net.WsaError.wsa_ipsec_name_policy_error) {
			return '(${err_code}) ${net.wsa_error(err_code)}'
		}
	}
	if err_code < 0 {
		return '0x${u32(err_code):08x}'
	}
	return '${err_code}'
}

fn vschannel_connect_error(err_code int) IError {
	if err_code == 0 {
		return error(vschannel_connect_failed_msg)
	}
	return error_with_code(vschannel_connect_failed_msg, err_code)
}

fn vschannel_should_report_connect_failure(err_code int) bool {
	if err_code == vschannel_sec_e_internal_error {
		return true
	}
	$if windows {
		return err_code in [int(net.WsaError.wsaenotconn), int(net.WsaError.wsaenetdown),
			int(net.WsaError.wsaenetunreach), int(net.WsaError.wsaetimedout),
			int(net.WsaError.wsaeconnrefused), int(net.WsaError.wsaehostdown),
			int(net.WsaError.wsaehostunreach), int(net.WsaError.wsahost_not_found),
			int(net.WsaError.wsatry_again), int(net.WsaError.wsano_recovery),
			int(net.WsaError.wsano_data)]
	}
	return false
}

fn vschannel_looks_like_connect_failure_response(response_text string) bool {
	trimmed := response_text.trim_space()
	return trimmed.starts_with('Error ') && (trimmed.contains('sending data to server')
		|| trimmed.contains('Error performing handshake'))
}

fn vschannel_request_error(err_code int) IError {
	if vschannel_should_report_connect_failure(err_code) {
		return vschannel_connect_error(err_code)
	}
	return error_with_code('http: vschannel request failed: ${vschannel_error_message(err_code)}',
		err_code)
}

fn vschannel_parse_response(response_text string, err_code int) !Response {
	if response_text.len < 5 || response_text[..5].to_lower() != 'http/' {
		if vschannel_should_report_connect_failure(err_code)
			|| vschannel_looks_like_connect_failure_response(response_text) {
			return vschannel_connect_error(err_code)
		}
	}
	return parse_response(response_text)
}
