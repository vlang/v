pub struct EncodeOptions[T] {
	payload   T
	key       string
	algorithm string = 'HS256'
}

pub fn encode[T](options EncodeOptions[T]) !string {
	return 'test'
}

fn test_generic_fn_with_generic_struct_init_syntax() {
	payload := {
		'test': 'test'
	}
	ret := encode(payload: payload, key: 'test')!
	println(ret)
	assert ret == 'test'
}
