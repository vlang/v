import os

fn test_linux_vlibc_threads_atomics_and_futex_with_optional_libc() ! {
	$if linux && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'hosted_vlibc_threads_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(work, 'main.v')
		os.write_file(source, '
fn worker(address u64) {
	word := unsafe { &u32(address) }
	text := "thread payload".repeat(256)
	assert text.len == 3584
	unsafe { text.free() }
	assert vlibc_cas_u32(word, 17, 42)
}

fn main() {
	mut word := u32(0)
	vlibc_store_u32(&word, 17)
	mut thread, code := vlibc_thread_spawn(worker, u64(&word))
	assert code == .enoerror
	assert vlibc_thread_join(mut thread) == .enoerror
	assert vlibc_thread_join(mut thread) == .enoerror
	assert vlibc_load_u32(&word) == 42
	assert vlibc_sys_futex(&word, 129, 1, unsafe { nil }, unsafe { nil }, 0) == 0
	println("vlibc threads passed")
}
')!
		for index, flags in [['-vlibc'], ['-shim-libc'], ['-syscalls', '-d', 'vlibc'],
			['-syscalls', '-d', 'vlibc', '-no-system-libc']] {
			program := os.join_path(work, 'threads_${index}')
			mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none']
			command << flags
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${flags}: ${compiled.output}'
			result := os.exec([program])
			assert result.exit_code == 0, '${flags}: ${result.output}'
			assert result.output.trim_space() == 'vlibc threads passed'
		}
	}
}
