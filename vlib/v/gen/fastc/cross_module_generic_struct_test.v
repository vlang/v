module fastc

import os
import v.cmdexec

// Generic structs of every project module are specialized: `ds.List[Message]` in `main`,
// through an alias, from a generic static method, and with arguments from a third module or
// composite arguments, get their own layout and methods instead of the erased one.
fn test_fastc_specializes_generic_structs_across_modules() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_cross_module_generic_structs_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'ds')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'shapes')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'ds', 'ds.v'), 'module ds

pub struct Node[T] {
pub mut:
	value T
	next  &Node[T] = unsafe { nil }
}

pub struct List[T] {
pub mut:
	first ?&Node[T]
	last  ?&Node[T]
	len   int
}

pub fn List.create[T]() &List[T] {
	return &List[T]{}
}

pub fn (mut l List[T]) push(value T) {
	node := &Node[T]{
		value: value
	}
	if mut last := l.last {
		last.next = node
	} else {
		l.first = node
	}
	l.last = node
	l.len++
}

pub fn (l &List[T]) sum_with(f fn (T) int) int {
	mut total := 0
	mut node := l.first or { return 0 }
	for {
		total += f(node.value)
		if isnil(node.next) {
			break
		}
		node = node.next
	}
	return total
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'shapes', 'shapes.v'), 'module shapes

pub struct Rect {
pub:
	w int
	h int
}

pub fn (r Rect) area() int {
	return r.w * r.h
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import ds
import shapes

struct Message {
	id   int
	refs []int
}

type Inbox = ds.List[Message]

struct Worker {
mut:
	inbox &Inbox             = unsafe { nil }
	rects ds.List[shapes.Rect]
	sizes ds.List[[]int]
}

fn main() {
	mut w := Worker{
		inbox: ds.List.create[Message]()
	}
	w.inbox.push(Message{1, [2, 3]})
	w.inbox.push(Message{4, [5]})
	println(w.inbox.len)
	println(w.inbox.sum_with(fn (m Message) int {
		return m.id + m.refs.len
	}))
	if last := w.inbox.last {
		println(last.value.refs)
	}
	w.rects.push(shapes.Rect{2, 3})
	w.rects.push(shapes.Rect{4, 5})
	println(w.rects.sum_with(fn (r shapes.Rect) int {
		return r.area()
	}))
	w.sizes.push([1, 2, 3])
	println(w.sizes.sum_with(fn (xs []int) int {
		return xs.len
	}))
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
	assert run.output.trim_space() == '2\n8\n[5]\n26\n3', run.output
}
