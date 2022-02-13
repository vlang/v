import strings

fn main() {
	mut sb := strings.new_builder(4)
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	sb.write_string('abcd')
	println(sb.str())
}
