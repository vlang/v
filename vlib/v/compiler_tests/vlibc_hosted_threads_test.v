import os

fn test_linux_hosted_vlibc_threads_atomics_and_futex() ! {
	$if linux && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'hosted_vlibc_threads_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(work, 'main.v')
		os.write_file(source, '
fn worker(address u64) {
	word := unsafe { &u32(address) }
	assert vlibc_cas_u32(word, 17, 42)
}

fn main() {
	mut word := u32(0)
	vlibc_store_u32(&word, 17)
	mut thread, code := vlibc_thread_spawn(worker, u64(&word))
	assert code == .enoerror
	assert vlibc_thread_join(mut thread) == .enoerror
	assert vlibc_load_u32(&word) == 42
	assert vlibc_sys_futex(&word, 129, 1, unsafe { nil }, unsafe { nil }, 0) == 0
	println("hosted vlibc threads passed")
}
')!
		for syscalls in [false, true] {
			program := os.join_path(work, 'threads_${syscalls}')
			mut command := [@VEXE, '-new-compiler', '-gc', 'none', '-d', 'vlibc']
			if syscalls {
				command << '-syscalls'
			}
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, 'syscalls=${syscalls}: ${compiled.output}'
			result := os.exec([program])
			assert result.exit_code == 0, 'syscalls=${syscalls}: ${result.output}'
			assert result.output.trim_space() == 'hosted vlibc threads passed'
		}
	}
}
