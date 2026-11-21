module fastc

import os
import v.cmdexec

// A generic struct of one module specialized for a sum type of another, through a generic
// static constructor, names the same copy in every module.
fn test_fastc_specializes_generic_structs_for_sum_types_of_other_modules() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_specializes_generic_structs_for_sum_type_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'cli')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'vx')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'cli', 'cli.v'), "module cli

import vx

type InputEvent = vx.Key | vx.Mouse

pub fn run() ! {
	mut tty := vx.Tty{
		n: 1
	}
	mut loop := vx.Loop.new[InputEvent](mut tty)
	loop.post(vx.Key{3})
	loop.post(vx.Mouse{4})
	println(loop.count())
	match loop.queue.items[1] {
		vx.Mouse { println('mouse') }
		vx.Key { println('key') }
	}
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'vx', 'vx.v'), 'module vx

pub struct Tty {
pub mut:
	n int
}

pub struct EventQueue[T] {
pub mut:
	items []T
}

pub fn EventQueue.new[T](cap int) &EventQueue[T] {
	return &EventQueue[T]{
		items: []T{cap: cap}
	}
}

pub struct Loop[T] {
mut:
	tty &Tty = unsafe { nil }
pub mut:
	queue &EventQueue[T] = unsafe { nil }
}

pub fn Loop.new[T](mut tty Tty) &Loop[T] {
	return &Loop[T]{
		tty:   &tty
		queue: EventQueue.new[T](8)
	}
}

pub fn (mut l Loop[T]) post(e T) {
	l.queue.items << e
}

pub fn (l &Loop[T]) count() int {
	return l.queue.items.len + l.tty.n
}

pub struct Key {
pub:
	code int
}

pub struct Mouse {
pub:
	x int
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import cli

fn main() {
	cli.run() or { panic(err) }
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '3\nmouse', run.output
}
