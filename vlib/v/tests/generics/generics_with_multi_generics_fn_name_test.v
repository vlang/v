pub struct ValueWithErrors[T, E] {
pub:
	value  T
	errors E
}

@[inline]
pub fn (self ValueWithErrors[T, E]) has_errors() bool {
	return self.errors.len > 0
}

pub fn transform_and_validate[T](data map[string]string) ValueWithErrors[T, map[string][]IError] {
	mut new_object := T{}
	mut errors := map[string][]IError{}
	return ValueWithErrors[T, map[string][]IError]{
		errors: errors
		value:  new_object
	}
}

struct BasicStruct {
	str    string @[max_length: 15; min_length: 3; req]
	number int    @[max: 100; min: 3; req]
	b      bool   @[req]
}

fn test_generics_with_multi_generics_fn_name() {
	data_with_errors := transform_and_validate[BasicStruct]({
		'str':    '1234567890123456'
		'number': '11'
	})

	dump(data_with_errors)
	assert data_with_errors.errors.len == 0
}
