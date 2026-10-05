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
		'import sample
fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   fs := [T.\$method]
   _ = fs[0](&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
		'import sample
fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   fs := {"add": T.\$method}
   _ = (fs["add"] or { panic("missing") })(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
		'import sample
struct Holder[T] {
 @[required]
 f fn (mut T, int) int
}
fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   holder := Holder[T]{f: T.\$method}
   _ = holder.f(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
	] {
		invocations := if body.contains('_ = f(') {
			[body, body.replace('_ = f(', '_ = (f)(')]
		} else if body.contains('_ = fs[0](') {
			[body, body.replace('_ = fs[0](', '_ = (fs[0])(')]
		} else if body.contains('_ = holder.f(') {
			[body, body.replace('_ = holder.f(', '_ = (holder.f)(')]
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
struct Holder[T] {
 @[required]
 f fn (mut T, int) int
}
fn reflected[T](mut app T) int {
 mut result := 0
 \$for method in T.methods {
  \$if method.name == "add" {
   result = (T.\$method)(mut app, 2)
   fs := [T.\$method]
   result = fs[0](mut app, 2)
   fs_map := {"add": T.\$method}
   result = (fs_map["add"] or { panic("missing") })(mut app, 2)
   holder := Holder[T]{f: T.\$method}
   result = holder.f(mut app, 2)
   result = app.\$method(1)
   result = app.add(1)
  }
 }
 return result
}
fn main() {
 mut app := sample.App{base: 10}
 read := sample.App.one
 read_result := read(&app, 1)
 if read_result != 11 { panic("public read result") }
 write := sample.App.add
 write_result := write(mut app, 3)
 if write_result != 13 { panic("public write result") }
 reflected_result := reflected[sample.App](mut app)
 if reflected_result != 23 || app.base != 23 { panic("reflected carrier result") }
 println("OK")
}
')!
	public_result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', 'run',
		source_path])
	assert public_result.exit_code == 0, public_result.output
	assert public_result.output.trim_space() == 'OK', public_result.output
	for control_source in [
		'struct App { mut: value int }
fn (mut app App) add[Y](x int) []int { app.value += x; return [app.value] }
fn invoke[T](mut app T) {
 result := app.add[int](1)
 if result != [1] { panic("generic array result") }
}
fn main() {
 mut app := App{}
 invoke[App](mut app)
 if app.value != 1 { panic("generic array receiver") }
 println("generic array receiver passed")
}
',
		'struct App { mut: value int }
fn increment(y int) int { return y + 1 }
fn (mut app App) callback[Y](x int) fn (int) int { app.value += x; return increment }
fn invoke[T](mut app T) {
 f := app.callback[int](1)
 result := f(2)
 if result != 3 { panic("generic callback result") }
}
fn main() {
 mut app := App{}
 invoke[App](mut app)
 if app.value != 1 { panic("generic callback receiver") }
 println("generic callback receiver passed")
}
',
		'struct App { mut: value int }
fn (mut app App) add(x int) int { app.value += x; return app.value }
fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   f := app.\$method
   fs := [f]
   result := fs[0](1)
   if result != 1 { panic("bound local array result") }
  }
 }
}
fn main() {
 mut app := App{}
 reflected[App](mut app)
 if app.value != 1 { panic("bound local receiver") }
 println("bound local array passed")
}
',
	] {
		os.write_file(source_path, control_source)!
		control := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', 'run',
			source_path])
		assert control.exit_code == 0, control.output
		assert control.output.contains('passed'), control.output
	}
}
