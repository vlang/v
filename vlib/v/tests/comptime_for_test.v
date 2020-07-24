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

fn no_lines(s string) string { return s.replace('\n', ' ') }

fn test_comptime_for() {
	println(@FN)
	methods := ['run', 'method2', 'int_method1', 'int_method2']
	$for method in App.methods {
		println('  method: $method.name | ' + no_lines('$method'))
		assert method.name in methods
	}
}

fn test_comptime_for_with_if() {
	println(@FN)
	methods := ['int_method1', 'int_method2']
	$for method in App.methods {
		println('  method: ' + no_lines('$method'))
		$if method.@type is int {
			println(method.attrs)
			assert method.name in methods
		}
	}
}

fn test_comptime_for_fields() {
	println(@FN)
	$for field in App.fields {
		println('  field: $field.name | ' + no_lines('$field'))
		$if field.@type is string {
			assert field.name in ['a', 'b', 'g']
		}
		$if field.@type is f32 {
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
