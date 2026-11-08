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
