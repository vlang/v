module fastc

import os
import v.cmdexec

const nil_util_module = 'module util

pub struct Node {
pub:
	x int
}

pub fn is_null(p voidptr) bool {
	return isnil(p)
}

pub fn take(n &Node) int {
	return n.x
}
'

const nil_program_prelude = 'module main

import util

struct Node {
	x int
}

struct Handle {
mut:
	ptr  voidptr
	node &Node = unsafe { nil }
	cb   fn () = unsafe { nil }
}

struct Slots {
	ptr  voidptr
	node &Node
	next voidptr
}

fn take(n &Node) int {
	return n.x
}

fn is_null(p voidptr) bool {
	return isnil(p)
}
'

// fastc_build_directory_program builds `source` as the main.v of a directory program with the
// `util` module beside it, which FastC compiles against the real vlib. It returns the directory
// and the build result.
fn fastc_build_directory_program(name string, source string) (string, os.Result) {
	root := os.join_path(os.vtmp_dir(), 'fastc_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'util')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_${name}' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'util', 'util.v'), nil_util_module) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	return root, cmdexec.run(@VEXE, ['-b', 'fastc', '-o', os.join_path(root, 'program'), root])
}

// A program that imports `os` builds: vlib's os.Mmap.close assigns `nil` to voidptr fields
// outside `unsafe`, which V3's checker accepts, so FastC accepts it too with the real vlib.
fn test_fastc_builds_a_program_importing_os() {
	$if !macos && !linux {
		return
	}
	root, build := fastc_build_directory_program('import_os', 'module main

import os

struct Handle {
mut:
	ptr voidptr
}

fn main() {
	mut h := Handle{
		ptr: voidptr(&h)
	}
	h.ptr = nil
	println((os.getpid() > 0).str() + " " + isnil(h.ptr).str())
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	assert build.exit_code == 0, build.output
	run := cmdexec.run(os.join_path(root, 'program'), [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true true'
}

// `nil` outside `unsafe` is accepted as the whole value of a field declared `voidptr`, in an
// assignment or a keyed or positional struct literal, and of a `voidptr` argument to a function
// of this module or of an imported one, as V3 accepts it.
fn test_fastc_accepts_nil_for_a_declared_voidptr() {
	$if !macos && !linux {
		return
	}
	root, build := fastc_build_directory_program('nil_voidptr', nil_program_prelude + '
fn main() {
	mut h := Handle{}
	h.ptr = voidptr(&h)
	h.ptr = nil
	k := Handle{
		ptr: nil
	}
	s := Slots{nil, &Node{}, nil}
	println(isnil(h.ptr).str() + " " + isnil(k.ptr).str() + " " + is_null(nil).str() + " " +
		util.is_null(nil).str() + " " + isnil(s.ptr).str() + " " + isnil(s.node).str() + " " +
		isnil(s.next).str())
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	assert build.exit_code == 0, build.output
	run := cmdexec.run(os.join_path(root, 'program'), [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true true true true true false true'
}

// `nil` outside `unsafe` stays refused where V3 refuses it: as a non-void reference argument or
// field, and as a fn-typed field, which FastC stores as `voidptr` too.
fn test_fastc_refuses_nil_where_v3_does() {
	$if !macos && !linux {
		return
	}
	for name, statement in {
		'reference_argument':           'println(take(nil))'
		'imported_reference_argument':  'println(util.take(nil))'
		'reference_field':              'h.node = nil'
		'fn_field':                     'h.cb = nil'
		'reference_literal':            'h = Handle{node: nil}'
		'positional_reference_literal': 'println(isnil(Slots{nil, nil, nil}.ptr))'
	} {
		root, build := fastc_build_directory_program('nil_${name}', nil_program_prelude + '
fn main() {
	mut h := Handle{}
	${statement}
	println(isnil(h.ptr))
}
')
		os.rmdir_all(root) or {}
		assert build.exit_code != 0, name
		assert build.output.contains('`nil` outside an `unsafe` block'), '${name}: ${build.output}'
	}
}

fn test_fastc_callback_nil_arguments_match_the_c_backend() {
	$if !macos && !linux {
		return
	}
	root, fastc_build := fastc_build_directory_program('nil_callback_argument', nil_program_prelude + '
fn accept_callback(callback fn ()) bool { return isnil(callback) }
fn main() { println(accept_callback(nil)) }
')
	defer { os.rmdir_all(root) or {} }
	assert fastc_build.exit_code == 0, fastc_build.output
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'callback_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'true', run.output
	}
}
