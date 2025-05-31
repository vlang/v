import module_a
import module_b

pub interface MyInterface {
	module_a.Writer
	module_b.Reader
}

fn test_multiple_embed_external_interface() {
	println('abc')
	assert true
}
