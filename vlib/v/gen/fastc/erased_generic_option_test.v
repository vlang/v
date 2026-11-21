module fastc

import os
import v.cmdexec

// Option results of erased generic methods box the boxed `T` again. Fields of the generic
// type, pointers to it and aliases of it all keep `T` to unwrap them.
fn test_fastc_erased_generic_option_results() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_erased_generic_option_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'ds')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'ds', 'ds.v'), 'module ds

pub struct Queue[T] {
mut:
	items []T
}

pub fn (mut q Queue[T]) push(v T) {
	q.items << v
}

pub fn (mut q Queue[T]) pop() ?T {
	if q.items.len == 0 {
		return none
	}
	v := q.items[0]
	q.items.delete(0)
	return v
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import ds

struct Release {
	refs []int
}

struct Message {
	release Release
}

type Mailbox = ds.Queue[Message]

struct Worker {
mut:
	value   ds.Queue[Message]
	direct  &ds.Queue[Message] = unsafe { nil }
	mailbox &Mailbox           = unsafe { nil }
}

fn (mut w Worker) drain(name string) {
	for {
		message := w.value.pop() or { break }
		println('\${name} value \${message.release.refs}')
	}
	for {
		message := w.direct.pop() or { break }
		println('\${name} direct \${message.release.refs}')
	}
	for {
		message := w.mailbox.pop() or { break }
		for r in message.release.refs {
			println('\${name} mailbox \${r}')
		}
	}
	if message := w.direct.pop() {
		println(message.release.refs)
	} else {
		println('\${name} empty')
	}
}

fn main() {
	mut w := Worker{
		direct:  &ds.Queue[Message]{}
		mailbox: &ds.Queue[Message]{}
	}
	w.value.push(Message{Release{[1]}})
	w.direct.push(Message{Release{[2, 3]}})
	w.mailbox.push(Message{Release{[4, 5]}})
	w.drain('first')
}
") or { panic(err) }
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
	assert run.output.trim_space() == 'first value [1]\nfirst direct [2, 3]\nfirst mailbox 4\nfirst mailbox 5\nfirst empty', run.output
}
