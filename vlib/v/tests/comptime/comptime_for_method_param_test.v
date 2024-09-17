module main

struct Struct1 {
	num f64
}

fn (mut s Struct1) do_thing(a f32, b voidptr) bool {
	return false
}

fn register[T]() []string {
	mut args := []string{}
	$for method in T.methods {
		$for arg in method.params {
			$if arg.typ is f32 {
				args << 'f32: ${arg.name} ${typeof(arg.typ).name}'
			} $else $if arg.typ is voidptr {
				args << '&void: ${arg.name} ${typeof(arg.typ).name}'
			} $else {
			}
		}
	}
	return args
}

pub fn test_main() {
	args := register[Struct1]()
	assert args[0] == 'f32: a f32'
	assert args[1] == '&void: b voidptr'
}
