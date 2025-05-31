pub type Command = Assign | Call

pub struct Assign {}

pub struct Call {
mut:
	text string
}

fn test_main() {
	mut command_arr_ptr := [&Command(Call{})]
	mut command_arr_el_ptr := &command_arr_ptr[0] // type is && ?

	match mut command_arr_el_ptr {
		Call {
			command_arr_el_ptr.text = 'foo'
		}
		Assign {}
	}
	if mut command_arr_el_ptr is Call {
		assert command_arr_el_ptr.text == 'foo'
	} else {
		assert false
	}

	assert typeof(command_arr_el_ptr) == 'Call'
	assert typeof(command_arr_ptr) == '[]&Command'
}
