import os

const interface_pointer_args_vexe = @VEXE
const interface_pointer_args_v_dir = os.dir(os.dir(@FILE))
const interface_pointer_args_vlib_dir = os.dir(interface_pointer_args_v_dir)
const interface_pointer_args_v_src = os.join_path(interface_pointer_args_v_dir, 'v.v')

fn test_interface_pointer_arguments_preserve_value_and_storage_types() {
	work_dir := os.join_path(os.temp_dir(), 'v3_interface_pointer_args_${os.getpid()}')
	os.mkdir_all(work_dir) or { panic(err) }
	defer {
		os.rmdir_all(work_dir) or {}
	}
	v3_bin := os.join_path(work_dir, 'v3')
	// Invoke the new compiler directly so a fallback cannot hide these regressions.
	build := os.execute('"${interface_pointer_args_vexe}" -gc none -d ownership -path "${interface_pointer_args_vlib_dir}|@vlib|@vmodules" -o "${v3_bin}" "${interface_pointer_args_v_src}"')
	assert build.exit_code == 0, build.output

	// #28734: a value cast passed to &Iface must not gain an extra address-of.
	interface_pointer_args_run(v3_bin, work_dir, 'value_cast', "interface Widget {
	x int
}

struct Label {
	x int
}

fn addr(w &Widget) voidptr {
	return voidptr(w)
}

fn read_x(w &Widget) int {
	return w.x
}

fn make_label(mut calls int) &Label {
	calls++
	return &Label{ x: 42 }
}

fn main() {
	probe := &Label{ x: 42 }
	assert addr(Widget(probe)) != unsafe { nil }
	assert read_x(Widget(probe)) == 42
	assert read_x(&Widget(probe)) == 42
	assert read_x(probe) == 42
	value := Widget(probe)
	assert read_x(value) == 42
	mut calls := 0
	assert read_x(Widget(make_label(mut calls))) == 42
	assert calls == 1
	println('ok')
}
")

	// #28733: the loop binding has &&Widget storage, but reads as &Widget.
	interface_pointer_args_run(v3_bin, work_dir, 'mutable_pointer_loop', "interface Widget {
mut:
	draw()
}

interface Sized {
mut:
	set_size(n int)
}

struct Label {
mut:
	size int
}

fn (mut l Label) draw() {}

fn (mut l Label) set_size(n int) {
	l.size = n
}

fn resize(mut w Widget, n int) {
	if mut w is Sized {
		w.set_size(n)
	}
}

fn forward(mut w &Widget, n int) {
	resize(mut w, n)
}

fn main() {
	mut widgets := []&Widget{}
	mut first := &Label{}
	mut second := &Label{}
	widgets << first
	widgets << second
	for mut w in widgets {
		resize(mut w, 5)
	}
	assert first.size == 5
	assert second.size == 5
	mut boxed := &Widget(first)
	forward(mut boxed, 9)
	assert first.size == 9
	assert second.size == 5
	println('ok')
}
")

	// Keep module-qualified interface types and struct-field array iteration covered.
	module_dir := os.join_path(work_dir, 'qualified', 'widgets')
	os.mkdir_all(module_dir) or { panic(err) }
	os.write_file(os.join_path(module_dir, 'widgets.v'), 'module widgets

pub interface Widget {
mut:
	draw()
}

pub interface Sized {
mut:
	set_size(n int)
}

pub struct Label {
pub mut:
	size int
}

pub fn (mut l Label) draw() {}

pub fn (mut l Label) set_size(n int) {
	l.size = n
}

pub fn resize(mut w Widget, n int) {
	if mut w is Sized {
		w.set_size(n)
	}
}

pub fn addr(w &Widget) voidptr {
	return voidptr(w)
}

pub struct Window {
pub mut:
	children []&Widget
}

pub fn (mut window Window) arrange(n int) {
	for mut child in window.children {
		resize(mut child, n)
	}
}
') or {
		panic(err)
	}
	interface_pointer_args_run(v3_bin, work_dir, 'qualified', "module main

import widgets

fn main() {
	mut label := &widgets.Label{}
	mut window := widgets.Window{}
	window.children << label
	window.arrange(7)
	assert label.size == 7
	mut children := []&widgets.Widget{}
	children << label
	for mut child in children {
		widgets.resize(mut child, 11)
	}
	assert label.size == 11
	assert widgets.addr(widgets.Widget(label)) != unsafe { nil }
	println('ok')
}
")
}

fn interface_pointer_args_run(v3_bin string, work_dir string, name string, source string) {
	project := os.join_path(work_dir, name)
	os.mkdir_all(project) or { panic(err) }
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'interface_pointer_arguments' }\n") or {
		panic(err)
	}
	main_file := os.join_path(project, 'main.v')
	os.write_file(main_file, source) or { panic(err) }
	output := os.join_path(project, 'program')
	compile := os.execute('"${v3_bin}" -ownership -d ownership -nocache -no-parallel -o "${output}" "${main_file}"')
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	run := os.execute('"${output}"')
	assert run.exit_code == 0, '${name}: ${run.output}'
	assert run.output.trim_space() == 'ok', '${name}: ${run.output}'
}
