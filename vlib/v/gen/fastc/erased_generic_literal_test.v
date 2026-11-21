module fastc

import os
import v.cmdexec

// Generic structs outside the entry module keep one erased layout. Their literals,
// including multi-line field defaults, use that layout like field types do.
fn test_fastc_erased_generic_struct_literals() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_erased_generic_literal_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'heap')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'heap', 'heap.v'), 'module heap

pub struct Heap[T] {
pub mut:
	root &T = unsafe { nil }
	less fn (a &T, b &T) bool @[required]
}

pub fn (h Heap[T]) is_empty() bool {
	return isnil(h.root)
}

pub struct Box[T] {
pub mut:
	value T
	count int
}

pub fn new_box(count int) Box[int] {
	return Box[int]{
		count: count
	}
}

pub struct Timer {
pub:
	at int
}

fn timer_less(a &Timer, b &Timer) bool {
	return a.at < b.at
}

$if macos || linux {
	pub struct Loop {
	pub mut:
		timers Heap[Timer] = Heap[Timer]{
			less: timer_less
		}
		boxed  Box[int]    = Box[int]{count: 7}
	}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import heap

fn main() {
	l := heap.Loop{}
	a := heap.Timer{1}
	b := heap.Timer{2}
	println(l.timers.is_empty())
	println(l.timers.less(&a, &b))
	println(l.boxed.count)
	println(heap.new_box(3).count)
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
	assert run.output.trim_space() == 'true\ntrue\n7\n3', run.output
}
