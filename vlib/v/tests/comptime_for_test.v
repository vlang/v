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
	h u8
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

fn (mut app App) string_arg(x string) {
}

fn no_lines(s string) string {
	return s.replace('\n', ' ')
}

fn test_comptime_for() {
	println(@FN)
	methods := ['run', 'method2', 'int_method1', 'int_method2', 'string_arg']
	$for method in App.methods {
		// ensure each method is scoped under a new block in the generated code
		x := '  method: ${method.name} | ' + no_lines('${method}')
		println(x)
		assert method.name in methods
	}
}

fn test_comptime_for_with_if() {
	println(@FN)
	mut methods_found := map[string]int{}
	$for method in App.methods {
		println('  method: ' + no_lines('${method}'))
		$if method.typ is fn () {
			methods_found['fn()'] += 1
			assert method.name in ['run', 'method2']
		}
		$if method.typ is fn () int {
			methods_found['fn() int'] += 1
		}
		$if method.return_type is int {
			assert method.name in ['int_method1', 'int_method2']
		}
		$if method.typ is fn (string) {
			methods_found['fn(string)'] += 1
		}
		$if method.args[0].typ is string {
			assert method.name == 'string_arg'
		}
	}
	assert methods_found['fn()'] == 2
	assert methods_found['fn() int'] == 2
	assert methods_found['fn(string)'] == 1
}

fn test_comptime_for_fields() {
	println(@FN)
	mut fields_found := 0
	$for field in App.fields {
		println('  field: ${field.name} | ' + no_lines('${field}'))
		$if field.typ is string {
			assert field.name in ['a', 'b', 'g']
		}
		$if field.typ is f32 {
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
		if field.name == 'f' {
			assert sizeof(field) == 8
			assert isreftype(field) == false
			assert typeof(field).name == 'u64'
			fields_found++
		}
		if field.name == 'g' {
			assert typeof(field).name == 'string'
			assert isreftype(field) == true
			fields_found++
		}
	}
	assert fields_found == 2
}
