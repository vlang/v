struct App {
}

['foo/bar/three']
fn (mut app App) run() {
}

['attr2']
fn (mut app App) method2() {
}

fn (mut app App) int_method1() int {}
fn (mut app App) int_method2() int {}

fn test_comptime_for() {
	methods := ['run', 'method2', 'int_method1', 'int_method2']
	$for method in App {
		assert method in methods
	}
}

fn test_comptime_for_with_if() {
	methods := ['int_method1', 'int_method2']
	$for method in App if int {
		assert method in methods
	}
}
