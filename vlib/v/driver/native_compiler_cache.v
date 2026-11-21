module driver

import os
import v.pref

// The cache can rely on the platform toolchain and V's bundled TinyCC. An
// arbitrary command can choose different tools or options on every invocation
// while reporting the same --version. Its outputs stay local to the build.
fn v3_native_compiler_cacheable(compiler string) bool {
	path := os.find_abs_path_of_executable(compiler) or { return false }
	resolved := os.real_path(path)
	bundled := os.join_path('thirdparty', 'tcc', 'tcc.exe')
	if resolved != os.real_path(os.join_path(@VEXEROOT, bundled))
		&& resolved != os.real_path(os.join_path(os.dir(os.executable()), bundled)) {
		if !v3_system_compiler_path(path, resolved) {
			return false
		}
		// These settings can redirect a system driver to external tools.
		for name in ['GCC_EXEC_PREFIX', 'COMPILER_PATH', 'DEVELOPER_DIR', 'TOOLCHAINS'] {
			if os.getenv(name) != '' {
				return false
			}
		}
	}
	// Even trusted directories can hold a shell shim. Read only the file magic.
	magic := v3_read_file_start(resolved, 4).hex()
	return magic in ['7f454c46', 'cffaedfe', 'feedfacf', 'cafebabe', 'bebafeca', 'cafebabf', 'bfbafeca'] || magic.starts_with('4d5a')
}

fn v3_system_compiler_path(path string, resolved string) bool {
	if os.dir(path) !in ['/usr/bin', '/bin'] {
		return false
	}
	name := os.file_name(path)
	base := name.trim_right('0123456789').trim_right('-')
	if base !in ['cc', 'gcc', 'g++', 'c++', 'clang', 'clang++', 'tcc', 'tinyc'] {
		return false
	}
	dir := os.dir(resolved)
	// Debian and Ubuntu package Clang under /usr/lib/llvm-N/bin, with
	// links in /usr/bin. The other platform drivers live in /usr/bin.
	return dir in ['/usr/bin', '/bin'] || (dir.starts_with('/usr/lib/llvm-') && os.file_name(dir) == 'bin'
		&& os.dir(dir).all_after_last('llvm-').bytes().all(it.is_digit()))
}

// TinyCC's generated -B points at its own runtime files, not at an external
// compiler or assembler. User-supplied -B is still checked before cache setup.
fn v3_native_compiler_args_cacheable(compiler string, args []string) bool {
	tinycc := pref.ccompiler_is_tinycc(compiler)
	return !args.any(v3_native_compiler_option_is_opaque(it, tinycc))
}

fn v3_native_compiler_option_is_opaque(flag string, tinycc bool) bool {
	clean := flag.trim_space()
	return v3_native_flag_is_opaque(clean)
		|| (!tinycc && clean.starts_with('-B') && clean !in ['-Bstatic', '-Bdynamic'])
}
