module main

struct Decoder {}

pub fn decode[T](mut result T) ! {
	mut decoder := Decoder{}
	decoder.decode_value(mut result)!
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.indirections != 0 {
		unsafe {
			*val = 2
		}
	} $else {
		unsafe {
			*val = 1
		}
	}
}

fn test_generic_mut_pointer_param() {
	mut value := 0
	mut result := &value
	decode[&int](mut result)!
	assert value == 2
}
