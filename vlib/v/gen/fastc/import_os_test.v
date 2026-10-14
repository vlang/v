module fastc

import os
import v.cmdexec

const nil_program_prelude = 'module main

struct Node {
	x int
}

struct Handle {
mut:
	ptr  voidptr
	node &Node = unsafe { nil }
	cb   fn () = unsafe { nil }
}

fn take(n &Node) int {
	return n.x
}

fn is_null(p voidptr) bool {
	return isnil(p)
}
'

// fastc_build_directory_program builds `source` as the main.v of a directory program, which
// FastC compiles against the real vlib. It returns the directory and the build result.
fn fastc_build_directory_program(name string, source string) (string, os.Result) {
	root := os.join_path(os.vtmp_dir(), 'fastc_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_${name}' }") or {
		panic(err)
	}
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
// assignment or a struct literal, and of a `voidptr` argument, as V3 accepts it.
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
	println(isnil(h.ptr).str() + " " + isnil(k.ptr).str() + " " + is_null(nil).str())
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	assert build.exit_code == 0, build.output
	run := cmdexec.run(os.join_path(root, 'program'), [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true true true'
}

// `nil` outside `unsafe` stays refused where V3 refuses it: as a non-void reference argument or
// field, and as a fn-typed field, which FastC stores as `voidptr` too.
fn test_fastc_refuses_nil_where_v3_does() {
	$if !macos && !linux {
		return
	}
	for name, statement in {
		'reference_argument': 'println(take(nil))'
		'reference_field':    'h.node = nil'
		'fn_field':           'h.cb = nil'
		'reference_literal':  'h = Handle{node: nil}'
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
