struct App {
	a string
	b string
mut:
	c int
	d f32
pub:
	e f32
	f u64
pub mut:
	g string
	h byte
}

['foo/bar/three']
fn (mut app App) run() {
}

['attr2']
fn (mut app App) method2() {
}

fn (mut app App) int_method1() int {
	return 0
}
fn (mut app App) int_method2() int {
	return 1
}

fn test_comptime_for() {
	methods := ['run', 'method2', 'int_method1', 'int_method2']
	$for method in App(methods) {
		println('method.attrs: $method.attrs')
		println('method.ret_type: $method.ret_type')
		println('method.name: $method.name')
		assert method.name in methods
	}
}

fn test_comptime_for_with_if() {
	methods := ['int_method1', 'int_method2']
	$for method in App(methods) {
		$if ret_type is int {
			println(method.attrs)
			assert method.name in methods
		}
	}
}

fn test_comptime_for_fields() {
	$for field in App(fields) {
		$if ret_type is string {
			assert field.name in ['a', 'b', 'g']
		}
		$if ret_type is f32 {
			assert field.name in ['d', 'e']
		}
		if field.is_mut {
			assert field.name in ['c', 'd', 'g', 'h']
		}
		if field.is_pub {
			assert field.name in ['e', 'f', 'g', 'h']
		}
		if field.is_pub && field.is_mut {
			assert field.name in ['g', 'h']
		}
	}
}
