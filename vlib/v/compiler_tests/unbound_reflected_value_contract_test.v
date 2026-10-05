module main

import os

fn test_unbound_reflected_values_enforce_privacy_and_mut_arguments() {
	root := os.join_path(os.vtmp_dir(), 'unbound_value_contract_${os.getpid()}')
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
	private_result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-o',
		c_path, source_path])
	assert private_result.exit_code != 0, private_result.output
	assert private_result.output.contains('method `sample.App.secret` is private'), private_result.output

	for body in [
		'struct LocalApp { mut: value int }
fn (mut app LocalApp) add(x int) int { app.value += x; return app.value }
fn main() { mut app := LocalApp{}; f := LocalApp.add; _ = f(&app, 1) }',
		'import sample
fn main() { mut app := sample.App{}; f := sample.App.add; _ = f(&app, 1) }',
		'import sample
fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   f := T.\$method
   _ = f(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
		'import sample
fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   _ = (T.\$method)(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
	] {
		invocations := if body.contains('_ = f(') {
			[body, body.replace('_ = f(', '_ = (f)(')]
		} else {
			[body]
		}
		for invocation in invocations {
			os.write_file(source_path, invocation + '\n')!
			missing_mut := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache',
				'-o', c_path, source_path])
			assert missing_mut.exit_code != 0, missing_mut.output
			assert missing_mut.output.contains('is `mut`, so use'), missing_mut.output
		}
	}
	os.write_file(source_path, 'import sample
fn reflected[T](mut app T) int {
 mut result := 0
 \$for method in T.methods {
  \$if method.name == "add" {
   result = (T.\$method)(mut app, 2)
  }
 }
 return result
}
fn main() {
 mut app := sample.App{base: 10}
 read := sample.App.one
 assert read(&app, 1) == 11
 write := sample.App.add
 assert write(mut app, 3) == 13
 assert reflected[sample.App](mut app) == 15
 assert app.base == 15
 println("OK")
}
')!
	public_result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', 'run',
		source_path])
	assert public_result.exit_code == 0, public_result.output
	assert public_result.output.trim_space() == 'OK', public_result.output
}
