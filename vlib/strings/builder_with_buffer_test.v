import strings
import benchmark

const max_iterations = 10

fn test_buf_memory() {
	mut new_sb := strings.new_builder_with_buffer([]u8{cap: 3})
	mut new_sb2 := strings.new_builder_with_buffer([]u8{cap: 3})

	new_sb.write_string('87')
	new_sb2.write_string('87')

	assert ptr_str(new_sb.str().str) != ptr_str(new_sb2.str().str)

	unsafe { new_sb.free() }
}

fn test_compatibility() {
	mut new_sb := strings.new_builder_with_buffer([]u8{cap: 2 * max_iterations + 1})

	mut b := benchmark.start()

	for _ in 0 .. max_iterations {
		new_sb.write_string('87')
	}
	str_from_new_sb := new_sb.str()
	b.measure('new_builder_with_buffer([]u8{cap: n})')

	mut old_sb := strings.new_builder(3 * max_iterations)
	for _ in 0 .. max_iterations {
		old_sb.write_string('87')
	}

	str_from_old_sb := old_sb.str()

	unsafe { old_sb.free() }
	b.measure('strings.new_builder(n)')
	assert str_from_old_sb == str_from_new_sb

	unsafe { new_sb.free() }
}
