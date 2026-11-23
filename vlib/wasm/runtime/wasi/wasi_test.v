module wasi

import os
import wasm
import wasm.runtime

fn test_wasi_arguments_environment_and_exit() ! {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, 1)
	for name in ['args_sizes_get', 'args_get', 'environ_sizes_get'] {
		m.new_function_import('wasi_snapshot_preview1', name, [.i32_t, .i32_t], [.i32_t])
	}
	m.new_function_import('wasi_snapshot_preview1', 'proc_exit', [.i32_t], [])
	mut f := m.new_function('args', [], [])
	f.i32_const(0)
	f.i32_const(4)
	f.call_import('wasi_snapshot_preview1', 'args_sizes_get')
	f.drop()
	f.i32_const(8)
	f.i32_const(64)
	f.call_import('wasi_snapshot_preview1', 'args_get')
	f.drop()
	f.i32_const(24)
	f.i32_const(28)
	f.call_import('wasi_snapshot_preview1', 'environ_sizes_get')
	f.drop()
	m.commit(f, true)
	mut exit_fn := m.new_function('exit', [.i32_t], [])
	exit_fn.local_get(0)
	exit_fn.call_import('wasi_snapshot_preview1', 'proc_exit')
	m.commit(exit_fn, true)
	mod := runtime.load(m.compile())!
	mut vm := runtime.instantiate(mod, imports(['guest.wasm', 'hello world']), runtime.Config{})!
	vm.call('args', [])!
	meta := vm.read_memory(0, 32)!
	assert get_u32(meta, 0) == 2
	assert get_u32(meta, 4) == 23
	assert get_u32(meta, 8) == 64
	assert get_u32(meta, 12) == 75
	assert get_u32(meta, 24) == 0
	assert get_u32(meta, 28) == 0
	assert vm.read_memory(64, 23)!.bytestr() == 'guest.wasm\x00hello world\x00'
	mut exited := false
	vm.call('exit', [runtime.Value.i32(7)]) or {
		assert err is Exit
		assert err.code() == 7
		exited = true
	}
	assert exited
	vm.call('args', [])!
}

fn test_wasi_read_skips_empty_buffers_and_reports_eof() ! {
	path := os.join_path(os.vtmp_dir(), 'wasi_stdin_${os.getpid()}')
	os.write_file(path, 'abc')!
	defer { os.rm(path) or {} }
	mut input := os.open(path)!
	defer { input.close() }

	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, 1)
	mut vm := runtime.instantiate(runtime.load(m.compile())!, [], runtime.Config{})!
	// An empty iovec, a buffer that fits the input exactly, and a buffer that sees EOF.
	put_u32(mut vm, 8, 64)!
	put_u32(mut vm, 12, 3)!
	put_u32(mut vm, 16, 67)!
	put_u32(mut vm, 20, 2)!
	args := [runtime.Value.i32(0), runtime.Value.i32(0), runtime.Value.i32(3), runtime.Value.i32(24)]
	assert read_input(mut vm, args, input)![0].as_i32() == 0
	assert get_u32(vm.read_memory(24, 4)!, 0) == 3
	assert vm.read_memory(64, 5)! == [u8(`a`), `b`, `c`, 0, 0]

	// A read that starts at EOF must still store a zero byte count.
	assert read_input(mut vm, args, input)![0].as_i32() == 0
	assert get_u32(vm.read_memory(24, 4)!, 0) == 0
}
