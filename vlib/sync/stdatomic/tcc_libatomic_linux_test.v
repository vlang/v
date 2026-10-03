import os
import rand

const vexe = @VEXE

const stdatomic_program = '
module main

import sync.stdatomic

fn main() {
	C.atomic_thread_fence(C.memory_order_seq_cst)
	mut a := stdatomic.new_atomic(i64(0))
	a.store(1)
	println(a.load())
	assert a.compare_and_swap(1, 2)
	println(a.load())
	println(a.add(3))
	println(a.load())
}
'

// This is a regression test for https://github.com/vlang/v/issues/28043 (and similar reports),
// where TCC failed to link sync.stdatomic programs on Linux/amd64 systems with newer GCC
// versions (e.g. GCC 15/16 on Arch Linux).
fn test_tcc_can_link_sync_stdatomic_programs_on_linux() {
	$if !(linux && (amd64 || arm64)) {
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_stdatomic_tcc_libatomic_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	vcache := os.join_path(workdir, 'vcache')
	os.mkdir_all(vcache) or { panic(err) }
	src := os.join_path(workdir, 'main.c.v')
	out := os.join_path(workdir, 'main')
	os.write_file(src, stdatomic_program) or { panic(err) }

	for gc_mode in ['none', 'boehm'] {
		prealloc_flag := if gc_mode == 'none' { '-prealloc' } else { '' }
		compile_cmd := 'env VCACHE=${os.quoted_path(vcache)} ${os.quoted_path(vexe)} -nocache -gc ${gc_mode} ${prealloc_flag} -cc tcc -no-retry-compilation -showcc -o ${os.quoted_path(out)} ${os.quoted_path(src)}'
		compile_res := os.exec(['env', 'VCACHE=' + '${vcache}', vexe, '-nocache', '-gc', '${gc_mode}',
			...(os.split_args(prealloc_flag) or { panic(err) }), '-cc', 'tcc', '-no-retry-compilation',
			'-showcc', '-o', '${out}', '${src}'])
		if compile_res.exit_code != 0 {
			panic('tcc compilation of a sync.stdatomic program failed (fallback to another compiler is disabled):\ncmd: ${compile_cmd}\noutput:\n${compile_res.output}')
		}
		assert !compile_res.output.contains('implicit tcc could not be used')
		$if !musl ? {
			assert !compile_res.output.contains('libatomic.a')
		}

		run_res := os.exec([out])
		assert run_res.exit_code == 0
		assert run_res.output.trim_space() == '1\n2\n2\n5'
	}
}
