module main

import os

fn test_unbound_imported_method_values_are_plain_function_pointers_and_respect_privacy() {
	root := os.join_path(os.vtmp_dir(), 'unbound_method_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'sample'))!
	defer { os.rmdir_all(root) or {} }
	source_path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(os.join_path(root, 'sample', 'sample.v'), 'module sample
pub struct App {
pub mut:
 base int
}
pub fn (app &App) one(x int) int { return app.base + x }
pub fn (mut app App) add(x int) int { app.base += x; return app.base }
fn (app &App) secret(x int) int { return app.base - x }
')!
	os.write_file(source_path, 'import sample
fn main() {
 f := sample.App.one
 app := sample.App{base: 10}
 assert f(&app, 1) == 11
 println("OK")
}
')!
	generated := os.exec([@VEXE, '-new-compiler', '-o', c_path, source_path])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_path)!
	assert !source.contains('_mvctx_'), source
	assert !source.contains('_mvwrap_'), source
	run := os.exec([@VEXE, '-new-compiler', 'run', source_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK', run.output
	os.write_file(source_path, 'import sample
fn main() { _ = sample.App.secret }
')!
	private_result := os.exec([@VEXE, '-new-compiler', '-check', source_path])
	assert private_result.exit_code != 0, private_result.output
	assert private_result.output.contains('method `sample.App.secret` is private'), private_result.output
	os.write_file(source_path, 'import sample
fn reflected[T](app &T) int {
 mut result := 0
 \$for method in T.methods {
  \$if method.name == "secret" {
   f := T.\$method
   result = f(app, 1)
  }
 }
 return result
}
fn main() { app := sample.App{}; println(reflected[sample.App](&app)) }
')!
	reflected_private := os.exec([@VEXE, '-new-compiler', '-o', c_path, source_path])
	assert reflected_private.exit_code != 0, reflected_private.output
	assert reflected_private.output.contains('method `sample.App.secret` is private'), reflected_private.output
	for body in [
		'fn main() {
 mut app := sample.App{}
 f := sample.App.add
 _ = f(&app, 1)
}',
		'fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   f := T.\$method
   _ = f(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
	] {
		for invocation in [body, body.replace('_ = f(', '_ = (f)(')] {
			os.write_file(source_path, 'import sample\n${invocation}\n')!
			missing_mut := os.exec([@VEXE, '-new-compiler', '-o', c_path, source_path])
			assert missing_mut.exit_code != 0, missing_mut.output
			assert missing_mut.output.contains('is `mut`, so use'), missing_mut.output
		}
	}
}
