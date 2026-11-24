import os

// Programs may declare waitpid with C's own status type. os.Process must
// still compile against that declaration: GCC 14 rejects an i64 status
// passed where the declaration says int pointer.
fn C.waitpid(pid i32, status &i32, options i32) i32

fn test_process_exit_code_with_a_program_waitpid_declaration() {
	mut p := os.new_process(os.find_abs_path_of_executable('sh')!)
	p.set_args(['-c', 'exit 3'])
	p.run()
	p.wait()
	assert p.code == 3
	p.close()
}
