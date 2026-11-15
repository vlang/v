@[has_globals]
module asn1

import goroutines
import sync.stdatomic
import time

@[heap]
struct NestingDepthTestArgs {
	bytes      []u8
	use_parser bool
	result     &stdatomic.AtomicVal[int]
}

__global active_nesting_depth_test_args = []&NestingDepthTestArgs{}

fn decode_nesting_depth_test_case(arg voidptr) {
	unsafe {
		args := &NestingDepthTestArgs(arg)
		args.result.store(decode_nesting_depth_test_result(args.bytes, args.use_parser))
	}
}

fn decode_nesting_depth_test_result(bytes []u8, use_parser bool) int {
	if use_parser {
		mut p := Parser.new(bytes)
		_ := p.read_tlv() or {
			return if err.msg().contains('maximum nesting depth') { 2 } else { 3 }
		}
	} else {
		_ := decode(bytes) or {
			return if err.msg().contains('maximum nesting depth') { 2 } else { 3 }
		}
	}
	return 1
}

fn decode_on_managed_stack(bytes []u8, use_parser bool) !int {
	result := stdatomic.new_atomic[int](0)
	args := &NestingDepthTestArgs{
		bytes:      bytes
		use_parser: use_parser
		result:     result
	}
	// Keep the callback data rooted for process lifetime, including on timeout.
	active_nesting_depth_test_args << args
	goroutines.goroutine_create(voidptr(decode_nesting_depth_test_case), voidptr(args), 0)
	for _ in 0 .. 1000 {
		state := result.load()
		if state != 0 {
			return state
		}
		time.sleep(1 * time.millisecond)
	}
	return error('managed goroutine did not complete ASN.1 decode in time')
}

fn test_sequence_and_set_nesting_limit_on_managed_stack() {
	for tag in [u8(0x30), 0x31] {
		at_limit := nested_der(tag, max_nesting_depth)
		over_limit := nested_der(tag, max_nesting_depth + 1)
		for use_parser in [true, false] {
			assert decode_on_managed_stack(at_limit, use_parser)! == 1
			assert decode_on_managed_stack(over_limit, use_parser)! == 2
		}
	}
}

fn nested_der(tag u8, depth int) []u8 {
	mut encoded := [u8(0x05), 0x00]
	for _ in 0 .. depth {
		encoded = wrap_test_der(tag, encoded)
	}
	return encoded
}

fn wrap_test_der(tag u8, content []u8) []u8 {
	mut wrapped := [tag]
	wrapped << encode_test_der_length(content.len)
	wrapped << content
	return wrapped
}

fn encode_test_der_length(value int) []u8 {
	if value < 128 {
		return [u8(value)]
	}
	mut octets := []u8{}
	mut remaining := value
	for remaining > 0 {
		octets << u8(remaining & 0xff)
		remaining >>= 8
	}
	mut encoded := [u8(0x80 | octets.len)]
	for i := octets.len - 1; i >= 0; i-- {
		encoded << octets[i]
	}
	return encoded
}
