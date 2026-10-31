module grpc

// Code is a gRPC status code, carried in the grpc-status trailer as an integer.
pub enum Code {
	ok                  = 0
	cancelled           = 1
	unknown             = 2
	invalid_argument    = 3
	deadline_exceeded   = 4
	not_found           = 5
	already_exists      = 6
	permission_denied   = 7
	resource_exhausted  = 8
	failed_precondition = 9
	aborted             = 10
	out_of_range        = 11
	unimplemented       = 12
	internal            = 13
	unavailable         = 14
	data_loss           = 15
	unauthenticated     = 16
}

// code_from_int validates a grpc-status value; the spec reserves 0..16.
pub fn code_from_int(v int) !Code {
	if v < 0 || v > 16 {
		return error('grpc: invalid status code ${v}')
	}
	return unsafe { Code(v) }
}

// ErrorDetail is a typed error detail (a google.rpc.* message or any proto)
// carried on the wire as a Connect error `details` entry: type_name is the
// message's fully-qualified proto name, value its serialized bytes.
pub struct ErrorDetail {
pub:
	type_name string
	value     []u8
}

// Status is an RPC outcome: `ok` plus an empty message means success.
pub struct Status {
pub:
	code    Code
	message string
	details []ErrorDetail
}

pub fn (s Status) is_ok() bool {
	return s.code == .ok
}

// StatusError carries a non-OK RPC outcome as a V error
pub struct StatusError {
	Error
pub:
	status Status
}

pub fn (e StatusError) msg() string {
	if e.status.message == '' {
		return 'grpc: ${e.status.code}'
	}
	return 'grpc: ${e.status.code}: ${e.status.message}'
}

// grpc-message decoding (spec "Percent-Encoded"): %XX byte sequences,
// malformed ones pass through untouched
pub fn percent_decode(s string) string {
	mut out := []u8{cap: s.len}
	mut i := 0
	for i < s.len {
		if s[i] == `%` && i + 2 < s.len {
			hi := hex_val(s[i + 1])
			lo := hex_val(s[i + 2])
			if hi >= 0 && lo >= 0 {
				out << (u8(hi) << 4 | u8(lo))
				i += 3
				continue
			}
		}
		out << s[i]
		i++
	}
	return out.bytestr()
}

fn hex_val(c u8) int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a`) + 10
	}
	if c >= `A` && c <= `F` {
		return int(c - `A`) + 10
	}
	return -1
}

// percent_encode is the inverse used when a server writes grpc-message:
// printable ASCII (0x20..0x7E) passes through except `%`; every other byte
// becomes %XX with uppercase hex, matching what percent_decode reverses.
pub fn percent_encode(s string) string {
	mut out := []u8{cap: s.len}
	for i in 0 .. s.len {
		c := s[i]
		if c >= 0x20 && c <= 0x7e && c != `%` {
			out << c
		} else {
			out << `%`
			out << hex_digit(c >> 4)
			out << hex_digit(c & 0x0f)
		}
	}
	return out.bytestr()
}

fn hex_digit(n u8) u8 {
	return if n < 10 { `0` + n } else { `A` + (n - 10) }
}
