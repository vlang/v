module main

import os

fn check_direct_unbound_values_require_mut(root string, source_path string, c_path string) ! {
	bodies := {
		'local':    'struct LocalApp { mut: value int }
fn (mut app LocalApp) add(x int) int { app.value += x; return app.value }
fn main() { mut app := LocalApp{}; f := LocalApp.add; _ = f(&app, 1) }'
		'imported': 'import sample
fn main() { mut app := sample.App{}; f := sample.App.add; _ = f(&app, 1) }'
	}
	mut modules := []string{cap: 4}
	for kind, body in bodies {
		for i, invocation in [body, body.replace('_ = f(', '_ = (f)(')] {
			name := '${kind}_unbound_call_${i}'
			modules << name
			dir := os.join_path(root, name)
			os.mkdir(dir)!
			os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}\n\n${invocation}\n')!
		}
	}
	imports := modules.map('import ${it} as _')
	os.write_file(source_path, 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n')!
	result := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-nocolor', '-o', c_path,
		source_path])
	assert result.exit_code != 0, result.output
	mut missing_mut := map[string]bool{}
	for line in result.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		if message.contains('is `mut`, so use') {
			// Strip the line and column while preserving drive letters in the path.
			file := location.all_before_last(':').all_before_last(':')
			missing_mut[os.file_name(file)] = true
		}
	}
	for name in modules {
		assert '${name}.v' in missing_mut, '${name}: missing mut argument diagnostic\n${result.output}'
	}
}

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
	private_result := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-o', c_path, source_path])
	assert private_result.exit_code != 0, private_result.output
	assert private_result.output.contains('method `sample.App.secret` is private'), private_result.output

	check_direct_unbound_values_require_mut(root, source_path, c_path)!
	for body in [
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
			missing_mut := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-o', c_path,
				source_path])
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
struct ArrayApp { mut: value int }
fn (mut app ArrayApp) add[Y](x int) []int { app.value += x; return [app.value] }
fn invoke_array[T](mut app T) {
 result := app.add[int](1)
 if result != [1] { panic("generic array result") }
}
fn check_generic_array_receiver() {
 mut app := ArrayApp{}
 invoke_array[ArrayApp](mut app)
 if app.value != 1 { panic("generic array receiver") }
 println("generic array receiver passed")
}

struct CallbackApp { mut: value int }
fn increment(y int) int { return y + 1 }
fn (mut app CallbackApp) callback[Y](x int) fn (int) int { app.value += x; return increment }
fn invoke_callback[T](mut app T) {
 f := app.callback[int](1)
 result := f(2)
 if result != 3 { panic("generic callback result") }
}
fn check_generic_callback_receiver() {
 mut app := CallbackApp{}
 invoke_callback[CallbackApp](mut app)
 if app.value != 1 { panic("generic callback receiver") }
 println("generic callback receiver passed")
}

struct BoundApp { mut: value int }
fn (mut app BoundApp) add(x int) int { app.value += x; return app.value }
fn reflected_bound_array[T](mut app T) {
 \$for method in T.methods {
  \$if method.name == "add" {
   f := app.\$method
   fs := [f]
   result := fs[0](1)
   if result != 1 { panic("bound local array result") }
  }
 }
}
fn check_bound_local_array_receiver() {
 mut app := BoundApp{}
 reflected_bound_array[BoundApp](mut app)
 if app.value != 1 { panic("bound local receiver") }
 println("bound local array passed")
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
 check_generic_array_receiver()
 check_generic_callback_receiver()
 check_bound_local_array_receiver()
}
')!
	out := os.join_path(root, 'valid_calls')
	compile := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-o', out, source_path])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	expected := [
		'OK',
		'generic array receiver passed',
		'generic callback receiver passed',
		'bound local array passed',
	]
	assert run.output.trim_space().split_into_lines() == expected, run.output
}
