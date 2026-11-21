module fastc

import os
import v.cmdexec

// Methods of vlib generic structs that dispatch on `T` at compile time (`AtomicVal[u32].load()`,
// `ThreadLocalStorage[voidptr].set()`) run as instances: project code naming such a struct with
// concrete arguments gets a copy of it in its vlib module.
fn test_fastc_vlib_generic_struct_gets_instances() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_vlib_generic_struct_gets_instances_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import sync as vsync
import sync.stdatomic

struct Atlas {
mut:
	modified stdatomic.AtomicVal[u32]
	flag     stdatomic.AtomicVal[bool]
}

const current = $if wasm32_wasi {
	unsafe { nil }
} $else {
	vsync.new_tls[voidptr](unsafe { nil }) or { panic(err) }
}

fn main() {
	mut a := Atlas{}
	a.modified.store(3)
	a.modified.add(4)
	m := a.modified.load()
	println(m)
	a.flag.store(true)
	println(a.flag.load())
	mut t := current
	x := 5
	t.set(voidptr(&x)) or { panic(err) }
	v := t.get() or { panic(err) }
	println(v == voidptr(&x))
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
	assert run.output.trim_space() == '7\ntrue\ntrue', run.output
}
