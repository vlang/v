pub const global_text = c'Text'
pub const global_ref = &global_text
pub const global_copy = global_text

fn test_main() {
	local_copy := global_text
	println('${global_text}')
	println('${global_ref}')
	println('${local_copy}')
	println('${global_copy}')
	assert *global_copy == c'Text'
	assert **global_ref == c'Text'
	assert *global_text == c'Text'
}
