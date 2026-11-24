module json2

// vtest vflags: -no-retry-compilation -cc tcc -d no_backtrace -thread-stack-size 2097152

struct Heavy32 {
	next ?&Heavy32
	a    Any
	f00  int
	f01  int
	f02  int
	f03  int
	f04  int
	f05  int
	f06  int
	f07  int
	f08  int
	f09  int
	f10  int
	f11  int
	f12  int
	f13  int
	f14  int
	f15  int
	f16  int
	f17  int
	f18  int
	f19  int
	f20  int
	f21  int
	f22  int
	f23  int
	f24  int
	f25  int
	f26  int
	f27  int
	f28  int
	f29  int
	f30  int
	f31  int
}

fn mixed_typed_any_nested_input(levels int) string {
	mut parts := []string{cap: levels * 2 + 4}
	for _ in 0 .. levels {
		parts << '{"next":'
	}
	parts << '{"a":'
	parts << '['.repeat(levels)
	parts << '0'
	parts << ']'.repeat(levels)
	parts << '}'
	for _ in 0 .. levels {
		parts << '}'
	}
	return parts.join('')
}

fn decode_heavy32_in_worker(text string, options DecoderOptions, result chan string) {
	if _ := decode[Heavy32](text, options) {
		result <- ''
	} else {
		result <- err.msg()
	}
}

// Run with `./v -showcc test vlib/json2/mixed_typed_any_depth_test.v` to confirm
// the test flags select TCC and give its worker a 2 MiB stack.
fn test_mixed_typed_any_depth_does_not_exhaust_small_worker_stack() {
	text := mixed_typed_any_nested_input(201)
	for options in [DecoderOptions{}, DecoderOptions{ json5: true }] {
		result := chan string{cap: 1}
		go decode_heavy32_in_worker(text, options, result)
		error_message := <-result
		assert error_message == '', 'mixed typed/Any decode failed: ${error_message}'
	}
}
