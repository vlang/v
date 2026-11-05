import os

fn test_smartcasts_preserve_imported_embedded_field_names_and_storage() {
	root := os.join_path(os.vtmp_dir(), 'imported_embed_smartcast_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'model'))!
	os.mkdir_all(os.join_path(root, 'other'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'imported_embed_smartcast' }")!
	for name in ['model', 'other'] {
		os.write_file(os.join_path(root, name, 'context.v'), 'module ${name}
pub struct Context {
pub mut:
	width int
}
')!
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import model
import other

interface Device {
	width int
}

struct DrawContext {
	model.Context
}

struct OtherContext {
	other.Context
}

struct PointerContext {
	&model.Context
}

struct ShadowContext {
	model.Context
mut:
	width int
}

struct UI {
mut:
	device &Device
}

struct Canvas {
mut:
	ui &UI
	@[required]
	draw_fn fn (&model.Context)
}

fn observe(context &model.Context) {
	assert context.width == 17
}

fn increment(mut context model.Context) {
	context.width++
}

fn (mut canvas Canvas) check(expected &model.Context) {
	if mut canvas.ui.device is DrawContext {
		canvas.draw_fn(&canvas.ui.device.Context)
		snapshot := canvas.ui.device.Context
		assert snapshot.width == 17
		borrowed := &canvas.ui.device.Context
		assert borrowed == expected
		increment(mut canvas.ui.device.Context)
		assert borrowed.width == 18
		assert snapshot.width == 17
	} else {
		assert false
	}
}

fn (mut canvas Canvas) check_other() {
	if mut canvas.ui.device is OtherContext {
		borrowed := &canvas.ui.device.Context
		assert borrowed.width == 29
	} else {
		assert false
	}
}

fn (mut canvas Canvas) check_pointer(expected &model.Context) {
	if mut canvas.ui.device is PointerContext {
		borrowed := canvas.ui.device.Context
		assert borrowed == expected
		assert borrowed.width == 31
	} else {
		assert false
	}
}

fn (mut canvas Canvas) check_shadow() {
	if mut canvas.ui.device is ShadowContext {
		// A real field still wins over the imported embed's promoted field.
		assert canvas.ui.device.width == 41
		canvas.ui.device.width += 2
		assert canvas.ui.device.width == 43
		assert canvas.ui.device.Context.width == 17
	} else {
		assert false
	}
}

type Choice = DrawContext | OtherContext

struct State {
mut:
	choice Choice
}

fn check_sum(mut state State) {
	if mut state.choice is DrawContext {
		borrowed := &state.choice.Context
		assert borrowed.width == 47
		increment(mut state.choice.Context)
		assert borrowed.width == 48
	} else {
		assert false
	}
}

fn main() {
	mut device := &DrawContext{Context: model.Context{width: 17}}
	mut canvas := Canvas{ui: &UI{device: device}, draw_fn: observe}
	canvas.check(&device.Context)
	assert device.width == 18

	mut other_canvas := Canvas{
		ui: &UI{device: &OtherContext{Context: other.Context{width: 29}}}
		draw_fn: observe
	}
	other_canvas.check_other()

	context := &model.Context{width: 31}
	mut pointer_canvas := Canvas{
		ui: &UI{device: &PointerContext{Context: context}}
		draw_fn: observe
	}
	pointer_canvas.check_pointer(context)

	mut shadow_canvas := Canvas{
		ui: &UI{device: &ShadowContext{Context: model.Context{width: 17}, width: 41}}
		draw_fn: observe
	}
	shadow_canvas.check_shadow()

	mut state := State{choice: DrawContext{Context: model.Context{width: 47}}}
	check_sum(mut state)
}
")!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		root])
	assert result.exit_code == 0, result.output
}
