module grpc

import time
import net.http

fn test_encode_grpc_timeout_picks_finest_fitting_unit() {
	assert encode_grpc_timeout(0) == '0n'
	assert encode_grpc_timeout(-5) == '0n' // non-positive clamps to 0n
	assert encode_grpc_timeout(50 * time.nanosecond) == '50n'
	assert encode_grpc_timeout(5 * time.millisecond) == '5000000n' // 5e6 ns, still 8 digits
	assert encode_grpc_timeout(5 * time.second) == '5000000u' // ns overflows 8 digits -> micros
	assert encode_grpc_timeout(2 * time.minute) == '120000m' // -> millis
	assert encode_grpc_timeout(30 * time.hour) == '108000S' // -> seconds
}

fn test_call_options_compose_over_config() {
	mut cfg := CallConfig{}
	for o in [timeout(3 * time.second), header('a', '1'), metadata({
		'b': ['2']
		'c': ['3']
	})] {
		o(mut cfg)
	}
	assert cfg.timeout == 3 * time.second
	assert cfg.metadata == {
		'a': ['1']
		'b': ['2']
		'c': ['3']
	}
}

fn test_repeated_header_accumulates() {
	mut cfg := CallConfig{}
	for o in [header('k', 'first'), header('k', 'second')] {
		o(mut cfg)
	}
	// header() appends, so a repeated key keeps every value (multi-valued metadata)
	assert cfg.metadata['k'] == ['first', 'second']
}

fn test_response_metadata_drops_control_headers() {
	mut h := http.new_header()
	for k, v in {
		'content-type': 'application/grpc+proto'
		'grpc-status':  '0'
		'grpc-message': 'ok'
		'x-ratelimit':  '42'
		'x-trace-id':   'abc'
	} {
		h.add_custom(k, v) or { panic(err) }
	}
	m := response_metadata(h)
	assert 'grpc-status' !in m
	assert 'grpc-message' !in m
	assert 'content-type' !in m
	assert m['x-ratelimit'] == ['42']
	assert m['x-trace-id'] == ['abc']
}

fn test_transport_error_maps_deadline_vs_unavailable() {
	// the deadline_hit flag decides the code, not the error text
	assert transport_error(error('read timed out'), true).status.code == .deadline_exceeded
	assert transport_error(error('connection refused'), false).status.code == .unavailable
	assert transport_error(error('connection refused'), true).status.code == .deadline_exceeded
}
