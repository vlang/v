import os

fn test_execute_mutex_storage_satisfies_native_alignment() {
	root := os.join_path(os.vtmp_dir(), 'execute_mutex_alignment_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os
fn main() {
	result := os.exec(["printf", "mutex_probe"])
	assert result.exit_code == 0, result.output
	assert result.output == "mutex_probe"
}
')!
	c_path := os.join_path(root, 'main.c')
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-o', c_path,
		source])
	assert generated.exit_code == 0, generated.output
	c_source := os.read_file(c_path)!
	declarations := c_source.split_into_lines().filter(it.contains(' os__g_v_os_execute_mutex_storage['))
	assert declarations.len == 1, declarations.str()
	// Check the declared storage type, not its incidental linker placement.
	// Only the native C compiler reads pthread.h and determines its alignment.
	probe := os.join_path(root, 'alignment.c')
	os.write_file(probe, '#include <pthread.h>
typedef unsigned char u8;
typedef unsigned long long u64;
${declarations[0]}
_Static_assert(sizeof(os__g_v_os_execute_mutex_storage) >= sizeof(pthread_mutex_t),
    "execute mutex storage is too small");
_Static_assert(__alignof__(__typeof__(os__g_v_os_execute_mutex_storage)) >= __alignof__(pthread_mutex_t),
    "execute mutex storage is under-aligned");
int main(void) {
    pthread_mutex_t *mutex = (void *)&os__g_v_os_execute_mutex_storage[0];
    if (pthread_mutex_init(mutex, 0)) return 1;
    if (pthread_mutex_lock(mutex)) return 2;
    if (pthread_mutex_unlock(mutex)) return 3;
    return pthread_mutex_destroy(mutex) != 0;
}
')!
	executable := os.join_path(root, 'alignment')
	// `tinyc` is V's driver name, so use the host C compiler for the native ABI
	// probe while retaining TinyCC for the V runtime check below.
	native_cc := if @CCOMPILER == 'tinyc' { 'cc' } else { @CCOMPILER }
	compiled := os.exec([native_cc, '-pthread', '-o', executable, probe])
	assert compiled.exit_code == 0, compiled.output
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
	executed := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', @CCOMPILER, 'run',
		source])
	assert executed.exit_code == 0, executed.output
}
