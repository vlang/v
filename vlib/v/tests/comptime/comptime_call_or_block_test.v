pub struct App {
}

pub fn (mut app App) app_index() ! {
	return error('hhh')
}

pub fn (mut app App) no_error() {
}

fn test_main() {
	mut app := App{}
	mut ret2 := ''
	$for method in App.methods {
		$if method.is_pub {
			app.$method() or { ret2 = err.msg() }
			dump(ret2)
		}
	}
	assert ret2 == 'hhh'
}
