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
 {
  f := sample.App.one
  app := sample.App{base: 10}
  assert f(&app, 1) == 11
  println("OK")
 }
 {
  mut app := sample.App{base: 10}
  reflected_result := reflected[sample.App](mut app)
  if reflected_result != 20 || app.base != 20 { panic("reflected carrier result") }
  println("reflected OK")
 }
}
')!
	valid_bin := os.join_path(root, 'valid_calls')
	built := os.exec([@VEXE, '-b', 'c', '-o', valid_bin, source_path])
	assert built.exit_code == 0, built.output
	source := os.read_file(valid_bin + '.c')!
	assert !source.contains('_mvctx_'), source
	assert !source.contains('_mvwrap_'), source
	run := os.exec([valid_bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['OK', 'reflected OK'], run.output
	direct_calls := 'import sample
fn main() {
 _ = sample.App.secret
 mut app := sample.App{}
 f := sample.App.add
 _ = f(&app, 1)
 _ = (f)(&app, 1)
}
'
	os.write_file(source_path, direct_calls)!
	direct_result := os.exec([@VEXE, '-check', '-nocolor', source_path])
	assert direct_result.exit_code != 0, direct_result.output
	expected := {
		' _ = sample.App.secret': 'method `sample.App.secret` is private'
		' _ = f(&app, 1)':        'is `mut`, so use'
		' _ = (f)(&app, 1)':      'is `mut`, so use'
	}
	lines := direct_calls.split_into_lines()
	errors := direct_result.output.split_into_lines().filter(it.contains(': error: '))
	for statement, message in expected {
		line := lines.index(statement) + 1
		assert line > 0, statement
		location := 'main.v:${line}:'
		assert errors.any(it.contains(location) && it.contains(message)), 'expected ${message} at ${location}\n${direct_result.output}'
	}
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
	reflected_private := os.exec([@VEXE, '-o', c_path, source_path])
	assert reflected_private.exit_code != 0, reflected_private.output
	assert reflected_private.output.contains('method `sample.App.secret` is private'), reflected_private.output
	for body in [
		'fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   f := T.\$method
   _ = f(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
		'fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   _ = (T.\$method)(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
		'fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   fs := [T.\$method]
   _ = fs[0](&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
		'fn reflected[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   fs := {"add": T.\$method}
   _ = (fs["add"] or { panic("missing") })(&app, 1)
  }
 }
}
fn main() { mut app := sample.App{}; reflected[sample.App](mut app) }',
		'struct Holder[T] {
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
			os.write_file(source_path, 'import sample\n${invocation}\n')!
			missing_mut := os.exec([@VEXE, '-o', c_path, source_path])
			assert missing_mut.exit_code != 0, missing_mut.output
			assert missing_mut.output.contains('is `mut`, so use'), missing_mut.output
		}
	}
}
