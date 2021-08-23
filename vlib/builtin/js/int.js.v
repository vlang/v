module builtin

pub fn (i int) str() string {
	mut res := ''
	#res = new builtin.string( i )

	return res
}
