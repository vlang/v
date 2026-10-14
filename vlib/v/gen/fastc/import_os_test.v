module fastc

import os
import v.cmdexec

// A program that imports `os` builds: vlib's os.Mmap.close assigns `nil` to voidptr fields
// outside `unsafe`, which V3's checker accepts, so FastC accepts it too with the real vlib.
fn test_fastc_builds_a_program_importing_os() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_import_os_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_import_os' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

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
') or {
		panic(err)
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true true'
}
