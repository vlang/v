module c

import os

fn test_reflected_bound_method_with_scalar_return_remains_a_callback() {
	root := os.join_path(os.vtmp_dir(), 'reflected_method_value_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, 'struct App { mut: value int }
fn (mut app App) add(x int) int {
 app.value += x
 return app.value
}
fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   callback := app.\$method
   assert callback(1) == 1
   assert callback(2) == 3
  }
 }
}
fn main() {
 mut app := App{}
 reflected[App](mut app)
 assert app.value == 3
 println("passed")
}
')!
	for flags in [[]string{}, ['-prealloc']] {
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', ...flags,
			'run', source_path])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'passed', result.output
	}
}
