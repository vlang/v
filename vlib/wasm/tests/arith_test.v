import wasm
import os

/* const exe = os.find_abs_path_of_executable('wasm-validate') or {
	exit(0)
}

fn validate(mod []u8)! {
	/* mut proc := os.new_process(exe)
	proc.set_args(['-'])
	proc.set_redirect_stdio()
	proc.run()
	{
		os.fd_write(proc.stdio_fd[0], mod.bytestr())
		os.fd_close(proc.stdio_fd[0])
	}
	proc.wait()
	if proc.status != .exited {
		return error("wasm-validate exited abormally")
	}
	if proc.code != 0 {
		return error("wasm-validate exited with a non zero exit code")
	}
	proc.close() */
} */

fn test_add() {
	mut m := wasm.Module{}
	mut a1 := m.new_function('add', parameters: [.i32_t, .i32_t], results: [.i32_t])
	{
		a1.local_get(0)
		a1.local_get(1)
		a1.add(.i32_t)
	}
	m.commit(a1, true)
	mut a2 := m.new_function('sub', parameters: [.i32_t, .i32_t], results: [.i32_t])
	{
		a2.local_get(0)
		a2.local_get(1)
		a2.sub(.i32_t)
	}
	m.commit(a2, true)
	mut a3 := m.new_function('mul', parameters: [.i32_t, .i32_t], results: [.i32_t])
	{
		a3.local_get(0)
		a3.local_get(1)
		a3.mul(.i32_t)
	}
	m.commit(a3, true)
	mut a4 := m.new_function('div', parameters: [.i32_t, .i32_t], results: [.i32_t])
	{
		a4.local_get(0)
		a4.local_get(1)
		a4.div(.i32_t, true)
	}
	m.commit(a4, true)
	//validate(m.compile()) or { panic(err) }
}