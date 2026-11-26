import os
import v.cmdexec
import v.tempname

fn compile_gettid_fixture(src string, bin string) os.Result {
	return cmdexec.run_in_merged_with_env(@VEXE, [src, '-b', 'c', '-o', bin], '', {
		'VFLAGS': ''
	})
}

fn test_v_gettid_uses_declared_libc_compat_helper() {
	$if !linux {
		return
	}
	workspace := os.join_path(os.vtmp_dir(), 'v3_gettid_compat_${tempname.unique_token()}')
	os.mkdir(workspace)!
	defer { os.rmdir_all(workspace) or {} }
	src := os.join_path(workspace, 'main.v')
	bin := os.join_path(workspace, 'program')
	os.write_file(src, 'module main

#flag -Werror=implicit-function-declaration

fn main() {
	println(v_gettid().str())
}
') or {
		panic(err)
	}
	compile := compile_gettid_fixture(src, bin)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('static inline u32 v3_gettid(void)'), generated
	assert generated.contains('#elif defined(__arm__)'), generated
	assert generated.contains('#elif defined(__riscv) && __riscv_xlen == 64'), generated
	assert generated.contains('#elif defined(__loongarch_lp64)'), generated
	assert generated.contains('#elif defined(__s390x__)'), generated
	assert generated.contains('#define SYS_gettid 236'), generated
	assert generated.contains('#error unsupported Linux gettid syscall number for this architecture'), generated
	assert generated.contains('long syscall(long number, ...);'), generated
	assert generated.contains('syscall(SYS_gettid)'), generated
	assert !generated.contains('#include <sys/syscall.h>'), generated
	assert !generated.contains('__NR_gettid'), generated
	assert !generated.contains('u32 gettid(void);'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().len > 0, run.output
}

fn test_v_gettid_with_explicit_c_syscall_keeps_one_syscall_decl() {
	$if !linux {
		return
	}
	workspace := os.join_path(os.vtmp_dir(), 'v3_gettid_explicit_syscall_${tempname.unique_token()}')
	os.mkdir(workspace)!
	defer { os.rmdir_all(workspace) or {} }
	src := os.join_path(workspace, 'main.v')
	bin := os.join_path(workspace, 'program')
	os.write_file(src, 'module main

#flag -Werror=implicit-function-declaration

fn C.syscall(number i32, va ...voidptr) i32

fn main() {
	_ := C.syscall(-1)
	println(v_gettid().str())
}
') or {
		panic(err)
	}
	compile := compile_gettid_fixture(src, bin)
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('i32 syscall(i32 number, ...);'), generated
	assert generated.contains('static inline u32 v3_gettid(void)'), generated
	assert generated.contains('syscall(SYS_gettid)'), generated
	assert !generated.contains('long syscall(long number, ...);'), generated
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().len > 0, run.output
}
