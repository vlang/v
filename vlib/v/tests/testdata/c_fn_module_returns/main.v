import typed
import opaque
import fallback

fn main() {
	assert typed.index_of_lf('xy\nz'.bytes()) == 2
	assert typed.index_of_lf('xyz'.bytes()) == -1
	assert typed.return_type() == '&u8'
	assert opaque.contains_b('abc'.bytes())
	assert !opaque.contains_b('xyz'.bytes())
	assert opaque.return_type() == 'voidptr'
	assert fallback.contains_b('abc'.bytes())
	assert fallback.return_type() == 'voidptr'
	println('ok')
}
