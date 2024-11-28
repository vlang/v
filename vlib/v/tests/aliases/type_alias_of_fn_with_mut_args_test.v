type MutCallback = fn (mut []string)

fn mutate(mut ss []string, cb MutCallback) {
	cb(mut ss)
}

fn test_type_alias_of_fn_with_mut_args() {
	mut s := ['a']
	mutate(mut s, fn (mut ss []string) {
		ss << 'b'
	})
	assert s == ['a', 'b']
}
