module main

import os
import v.cmdexec

// These programs build with `-b arm64` and must print what the C backend
// prints. Each one is a project of several modules, because the SSA builder's
// name and layout lookups go wrong when modules share type, enum or function
// names, or size arrays with their own constants.

// run_arm64 writes `files` into a new project, builds its `main.v` with
// `-b arm64` and `args`, and returns the program's output.
fn run_arm64(name string, files map[string]string, args []string) string {
	root := os.join_path(os.vtmp_dir(), 'arm64_ssa_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }") or { panic(err) }
	for path, source in files {
		full := os.join_path(root, path)
		os.mkdir_all(os.dir(full)) or { panic(err) }
		os.write_file(full, source) or { panic(err) }
	}
	output := os.join_path(root, 'main')
	mut build := ['-b', 'arm64']
	build << args
	build << ['-o', output, os.join_path(root, 'main.v')]
	compiled := cmdexec.run(@VEXE, build)
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(output, []string{})
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// gg's `App` must not take over the layout of the program's own `App`.
fn test_module_struct_keeps_program_struct_name() {
	$if macos && arm64 {
		output := run_arm64('struct_names', {
			'widgets/widgets.v': "module widgets

pub struct App {
pub mut:
	backend int = 7
	queue   [8]u64
	title   string = 'widgets'
}

pub fn new_app() App {
	return App{}
}
"
			'main.v':            "module main

import widgets

struct Ctx {
mut:
	width int
}

struct App {
mut:
	ctx   &Ctx = unsafe { nil }
	image int
}

fn main() {
	mut app := &App{}
	app.ctx = &Ctx{
		width: 600
	}
	app.image = 3
	app.ctx.width += app.image
	println('\${app.ctx.width} \${app.image} \${widgets.new_app().title}')
}
"
		}, []string{})
		assert output == '603 3 widgets', output
	}
}

// `views.TextStyle` is a struct even though `styles.TextStyle` is an enum; taking
// it for one gave it the `i32` layout and then overwrote `i32` with the struct.
fn test_qualified_struct_is_not_another_modules_enum() {
	$if macos && arm64 {
		output := run_arm64('enum_names', {
			'styles/styles.v': 'module styles

pub enum TextStyle {
	plain
	bold
}
'
			'views/views.v':   "module views

import styles

pub struct TextStyle {
pub:
	size  f64 = 15.0
	name  string
	lines int = 1
}

fn styled(base TextStyle, bold bool) TextStyle {
	if bold {
		return TextStyle{
			...base
			lines: base.lines + 1
		}
	}
	return base
}

pub fn heading(size f64) TextStyle {
	return styled(TextStyle{
		size: size
		name: 'heading'
	}, true)
}

pub fn plain() styles.TextStyle {
	return .bold
}
"
			'main.v':          "module main

import views

fn scaled(v i32, by i32) i32 {
	return v * by
}

fn main() {
	style := views.heading(24.0)
	println('\${style.name} \${style.size} \${style.lines} \${int(views.plain())} \${scaled(i32(7), i32(6))}')
}
"
		}, []string{})
		assert output == 'heading 24.0 2 1 42', output
	}
}
