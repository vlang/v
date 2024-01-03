import strings
import benchmark

const max_iterations = 10
// const max_iterations = 100_000

// ./v -stats -prod test vlib/strings/builder_with_limit_test.v

fn test_compatibility() {
	mut new_sb := strings.new_builder_limited_by_fixed_array([2 * max_iterations + 1]u8{})

	mut b := benchmark.start()

	for _ in 0 .. max_iterations {
		new_sb.write_string('87')
	}
	str_from_new_sb := new_sb.str()
	b.measure('new_builder_limited_by_fixed_array([n]u8)')

	mut old_sb := strings.new_builder(3 * max_iterations)
	for _ in 0 .. max_iterations {
		old_sb.write_string('87')
	}

	str_from_old_sb := old_sb.str()

	unsafe { old_sb.free() }
	b.measure('strings.new_builder(n)')
	dump(str_from_old_sb == str_from_new_sb)
}
