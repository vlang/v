import os

const freestanding_vexe = @VEXE
const freestanding_tests_dir = os.dir(@FILE)
const freestanding_v3_dir = os.dir(freestanding_tests_dir)
const freestanding_vlib_dir = os.dir(freestanding_v3_dir)
const freestanding_v3_src = os.join_path(freestanding_v3_dir, 'v.v')

// Headers a C99 freestanding implementation has to provide. Everything else in
// the preamble belongs to a hosted libc.
const freestanding_c_headers = ['float.h', 'limits.h', 'stdbool.h', 'stddef.h', 'stdint.h']

// A sample of what a freestanding translation unit does not have.
const hosted_only_c_headers = ['sys/un.h', 'netdb.h', 'arpa/inet.h', 'sys/socket.h', 'termios.h',
	'dirent.h', 'signal.h', 'time.h', 'math.h']

const hosted_guard_line = '#if !defined(__STDC_HOSTED__) || __STDC_HOSTED__'

fn freestanding_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_freestanding_preamble_codegen_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${freestanding_vexe} -gc none -path "${freestanding_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${freestanding_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn freestanding_generate_c(v3_bin string, name string, target_os string, source string) (string, string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.write_file(src, source) or { panic(err) }
	generate := os.execute('${v3_bin} -os ${target_os} -nofloat -gc none -o ${c_path} ${src}')
	assert generate.exit_code == 0, generate.output
	return os.read_file(c_path) or { panic(err) }, c_path
}

fn includes_header(c_code string, header string) bool {
	for line in c_code.split_into_lines() {
		if line.trim_space() == '#include <${header}>' {
			return true
		}
	}
	return false
}

// Whether a libc exists is the C compiler's answer, not the target's: `-os vinix`
// serves both the kernel, which compiles -ffreestanding, and util-vinix, which is
// an ordinary hosted program. So the host headers stay in the preamble behind a
// guard the compiler evaluates, rather than the target picking the headerless
// preamble for both and conflicting with the include-based detection.
fn test_vinix_target_keeps_the_system_preamble_behind_a_hosted_guard() {
	v3_bin := freestanding_build_v3()
	c_code, _ := freestanding_generate_c(v3_bin, 'hosted_guarded', 'vinix', 'module main\n\n#include <symbols.h>\n\nfn main() {\n}\n')
	// The system preamble was chosen, not the headerless one: this is what a
	// hosted `-os vinix` program such as util-vinix needs.
	for header in hosted_only_c_headers {
		assert includes_header(c_code, header), 'the system preamble lost <${header}>'
	}
	assert c_code.contains(hosted_guard_line), 'the hosted headers are not guarded at all'
	for header in freestanding_c_headers {
		assert includes_header(c_code, header), 'no #include <${header}> was emitted'
	}
	// The project's own header is untouched.
	assert c_code.contains('#include <symbols.h>'), c_code#[0..2000]
}

// The point of the guard: the same generated C has to preprocess under the flags a
// kernel uses. -nostdlibinc drops the libc search path and keeps the compiler's own
// freestanding headers, which is the freestanding contract.
fn test_generated_c_preprocesses_for_a_freestanding_compile() {
	cc := os.find_abs_path_of_executable('clang') or {
		eprintln('> skipping: clang is needed to preprocess the generated C')
		return
	}
	v3_bin := freestanding_build_v3()
	c_code, c_path := freestanding_generate_c(v3_bin, 'freestanding_preprocess', 'vinix', 'module main\n\nfn main() {\n}\n')
	discard := os.join_path(os.temp_dir(), 'v3_freestanding_preprocess.i')
	preprocess := os.execute('${os.quoted_path(cc)} -ffreestanding -nostdlibinc -E ${os.quoted_path(c_path)} -o ${os.quoted_path(discard)}')
	assert preprocess.exit_code == 0, preprocess.output

	// And the guard is what makes that work: neutralise it and the same C fails.
	unguarded_path := os.join_path(os.temp_dir(), 'v3_freestanding_unguarded.c')
	os.write_file(unguarded_path, c_code.replace(hosted_guard_line, '#if 1')) or { panic(err) }
	without := os.execute('${os.quoted_path(cc)} -ffreestanding -nostdlibinc -E ${os.quoted_path(unguarded_path)} -o ${os.quoted_path(discard)}')
	assert without.exit_code != 0, 'the hosted headers resolved without the guard, so this proves nothing'
}

// A hosted target is unchanged: it still gets every header it did before.
fn test_hosted_target_still_includes_host_libc_headers() {
	v3_bin := freestanding_build_v3()
	c_code, _ := freestanding_generate_c(v3_bin, 'hosted_keeps_posix', 'linux', 'module main\n\n#include <stdio.h>\n\nfn main() {\n}\n')
	for header in hosted_only_c_headers {
		assert includes_header(c_code, header), 'a hosted target lost <${header}>'
	}
}
