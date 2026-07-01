import os

const gettid_compat_vexe = @VEXE
const gettid_compat_tests_dir = os.dir(@FILE)
const gettid_compat_v3_dir = os.dir(gettid_compat_tests_dir)
const gettid_compat_vlib_dir = os.dir(gettid_compat_v3_dir)
const gettid_compat_v3_src = os.join_path(gettid_compat_v3_dir, 'v3.v')

fn gettid_compat_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_gettid_compat_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${gettid_compat_vexe} -gc none -path "${gettid_compat_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${gettid_compat_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_v_gettid_uses_declared_libc_compat_helper() {
	$if !linux {
		return
	}
	v3_bin := gettid_compat_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_gettid_compat_${os.getpid()}.v')
	bin := os.join_path(os.temp_dir(), 'v3_gettid_compat_${os.getpid()}')
	os.write_file(src, 'module main

#flag -Werror=implicit-function-declaration

fn main() {
	println(v_gettid().str())
}
') or {
		panic(err)
	}
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('static inline u32 v3_gettid(void)'), generated
	assert generated.contains('#elif defined(__arm__)'), generated
	assert generated.contains('#elif defined(__riscv) && __riscv_xlen == 64'), generated
	assert generated.contains('#elif defined(__loongarch_lp64)'), generated
	assert generated.contains('#error unsupported Linux gettid syscall number for this architecture'), generated
	assert generated.contains('long syscall(long number, ...);'), generated
	assert generated.contains('syscall(SYS_gettid)'), generated
	assert !generated.contains('#include <sys/syscall.h>'), generated
	assert !generated.contains('__NR_gettid'), generated
	assert !generated.contains('u32 gettid(void);'), generated
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().len > 0, run.output
}

fn test_v_gettid_with_explicit_c_syscall_keeps_one_syscall_decl() {
	$if !linux {
		return
	}
	v3_bin := gettid_compat_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_gettid_explicit_syscall_${os.getpid()}.v')
	bin := os.join_path(os.temp_dir(), 'v3_gettid_explicit_syscall_${os.getpid()}')
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
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('i32 syscall(i32 number, ...);'), generated
	assert generated.contains('static inline u32 v3_gettid(void)'), generated
	assert generated.contains('syscall(SYS_gettid)'), generated
	assert !generated.contains('long syscall(long number, ...);'), generated
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().len > 0, run.output
}
