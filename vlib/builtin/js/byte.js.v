module builtin

pub fn (b byte) is_space() bool {
	mut result := false
	#result = /^\s*$/.test(String.fromCharCode(b))

	return result
}
