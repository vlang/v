import wasm
import os

const exe = os.find_abs_path_of_executable('wasm-validate') or { exit(0) }

fn validate(mod []u8) ! {
	mut proc := os.new_process(exe)
	proc.set_args(['-'])
	proc.set_redirect_stdio()
	proc.run()
	{
		os.fd_write(proc.stdio_fd[0], mod.bytestr())
		os.fd_close(proc.stdio_fd[0])
	}
	proc.wait()
	if proc.status != .exited {
		return error('wasm-validate exited abormally')
	}
	if proc.code != 0 {
		return error('wasm-validate exited with a non zero exit code')
	}
	proc.close()
}

fn test_globals() {
	mut m := wasm.Module{}

	vsp := m.new_global('__vsp', .i32_t, true, wasm.constexpr_value(10))
	mut func := m.new_function('vsp', [], [.i32_t])
	{
		func.global_get(vsp)
		func.i32_const(20)
		func.add(.i32_t)
		func.global_set(vsp)
		func.global_get(vsp)
	}
	m.commit(func, true)

	fref := m.new_global('__ref', .funcref_t, true, wasm.constexpr_ref_null(.funcref_t))
	mut func1 := m.new_function('ref', [], [])
	{
		func1.ref_func('vsp')
		func1.global_set(fref)
	}
	m.commit(func1, true)

	gimport := m.new_global_import('env', '__import', .f64_t, false)
	mut func2 := m.new_function('import', [], [.f64_t])
	{
		func2.global_get(gimport)
	}
	m.commit(func2, true)

	validate(m.compile()) or { panic(err) }
}
