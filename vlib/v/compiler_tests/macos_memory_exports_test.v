import os

// Execute the actual generated V byte helpers through their C ABI on Linux.
// Darwin startup, traps, and TLS registration have their own target-host tests.
fn test_macos_amd64_generated_v_memory_exports() ! {
	$if !linux {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'macos_v_memory_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'main.v')
	os.write_file(source, 'fn main() { println(bf_last_errno()) }\n')!
	for optimized in [false, true] {
		generated := os.join_path(work, 'macos.c')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-os', 'macos', '-arch',
			'amd64', '-freestanding', '-o', generated, source]
		if optimized { command << '-prod' }
		result := os.exec(command)
		assert result.exit_code == 0, result.output
		c_source := os.read_file(generated)!
		functions := macos_memory_functions(c_source)
		mut definitions := '#include <stddef.h>\n#include <stdint.h>\ntypedef uint8_t u8;\ntypedef int32_t i32;\ntypedef int64_t i64;\n'
		for cell in ['bf_native_errno_cell', 'bf_last_errno_cell'] {
			mut found := false
			for line in c_source.split_into_lines() {
				if line.starts_with('_Thread_local ') && line.contains(' ${cell}') {
					definitions += line + '\n'
					found = true
					break
				}
			}
			assert found, 'missing generated C TLS cell ${cell}'
		}
		for definition in functions {
			definitions += definition.all_before('{').trim_space() + ';\n'
		}
		for definition in functions { definitions += definition + '\n' }
		os.write_file(os.join_path(work, 'generated_memory.c'), definitions)!
		oracle := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'macos_memory_exports', 'oracle.c')
		binary := os.join_path(work, 'oracle')
		compiled := os.exec([cc, '-std=c11', '-D_GNU_SOURCE', '-ffreestanding', '-fno-builtin',
			'-fno-stack-protector', '-Wall', '-Wextra', '-Werror',
			if optimized { '-O3' } else { '-O0' }, '-I', work, oracle, '-pthread', '-o', binary])
		assert compiled.exit_code == 0, compiled.output
		run := os.exec([binary])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'generated V macOS memory C ABI and errno passed'
	}

	// These leaf operations must remain callable before profiling/tracing or TLS
	// initialization. Instrumented callers must not instrument the leaf chain.
	instrumented := os.join_path(work, 'instrumented.c')
	result := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-os', 'macos', '-arch',
		'amd64', '-freestanding', '-profile', '-trace-calls', '-o', instrumented, source])
	assert result.exit_code == 0, result.output
	for definition in macos_memory_functions(os.read_file(instrumented)!) {
		if definition.contains('bf_last_errno(') { continue }
		assert !definition.contains('_PROF_FN_START'), definition
		assert !definition.contains('trace_calls__on_call'), definition
	}

	// An embedding host owns its native errno accessor and native TLS.
	embedded := os.join_path(work, 'embedded.c')
	embedded_result := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-os', 'macos',
		'-arch', 'amd64', '-freestanding', '-d', 'v_bare_no_startup', '-o', embedded, source])
	assert embedded_result.exit_code == 0, embedded_result.output
	assert !os.read_file(embedded)!.contains('bare_macos_errno_pointer(')

	// The user-supported ARM64 platform keeps its libSystem implementations.
	arm := os.join_path(work, 'arm.c')
	arm_result := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-os', 'macos', '-arch',
		'arm64', '-freestanding', '-o', arm, source])
	assert arm_result.exit_code == 0, arm_result.output
	arm_source := os.read_file(arm)!
	assert !arm_source.contains('bare_macos_memcpy(')
	assert !arm_source.contains('bare_macos_errno_pointer(')
}

fn macos_memory_functions(source string) []string {
	mut result := []string{}
	for name in ['v_memcpy', 'v_memmove', 'v_memset', 'v_memcmp', 'v_strlen', 'bare_macos_memcpy',
		'bare_macos_memmove', 'bare_macos_memset', 'bare_macos_memcmp', 'bare_macos_strlen',
		'bare_macos_bzero', 'bare_macos_internal_bzero', 'bare_macos_errno_pointer', 'memcpy',
		'memmove', 'memset', 'memcmp', 'strlen', 'bzero', '__bzero', '__error', 'bf_last_errno'] {
		mut collecting := false
		mut definition := ''
		mut depth := 0
		for line in source.split_into_lines() {
			if !collecting && (line.contains(' ${name}(') || line.contains(' (${name})(')) && line.ends_with(' {') {
				collecting = true
			}
			if !collecting { continue }
			definition += line + '\n'
			depth += line.count('{') - line.count('}')
			if depth == 0 { break }
		}
		assert collecting && depth == 0, 'missing generated function ${name}'
		result << definition
	}
	return result
}

fn test_macos_amd64_shared_host_retains_its_c_errno() ! {
	$if !macos {
		return
	}
	$if !amd64 {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'macos_host_errno_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'library.c.v')
	os.write_file(source, '#include <errno.h>
fn C.__error() &i32
@[export: "read_host_errno"; no_profile; no_trace]
fn read_host_errno() i32 { return unsafe { *C.__error() } }
')!
	library := os.join_path(work, 'library.dylib')
	compiled := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', cc, '-freestanding',
		'-shared', '-d', 'v_bare_no_startup', '-o', library, source])
	assert compiled.exit_code == 0, compiled.output
	host_source := os.join_path(work, 'host.c')
	os.write_file(host_source, '#include <assert.h>
#include <dlfcn.h>
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
int main(int argc, char **argv) {
 assert(argc == 2);
 void *library = dlopen(argv[1], RTLD_NOW | RTLD_LOCAL);
 assert(library != 0);
 int32_t (*read_error)(void) = (int32_t (*)(void))dlsym(library, "read_host_errno");
 assert(read_error != 0);
 errno = EACCES;
 assert(read_error() == EACCES);
 errno = 0;
 assert(close(-1) == -1 && errno == EBADF);
 assert(read_error() == EBADF);
 assert(dlclose(library) == 0);
 return 0;
}
')!
	host := os.join_path(work, 'host')
	linked := os.exec([cc, '-Wall', '-Wextra', '-Werror', host_source, '-o', host])
	assert linked.exit_code == 0, linked.output
	run := os.exec([host, library])
	assert run.exit_code == 0, run.output
}
