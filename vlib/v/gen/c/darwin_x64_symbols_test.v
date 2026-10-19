module c

import os
import v.cmdexec
import v.pref

const darwin_x64_guard = '#if defined(__APPLE__) && defined(__x86_64__)'

// darwin_x64_program_symbols are the SDK-renamed functions that the program of
// darwin_x64_symbols_project reaches, through `os` and through its own bindings.
const darwin_x64_program_symbols = {
	'fdopendir': '_fdopendir$INODE64'
	'fstat':     '_fstat$INODE64'
	'lstat':     '_lstat$INODE64'
	'opendir':   '_opendir$INODE64'
	'readdir':   '_readdir$INODE64'
	'realpath':  '_realpath$DARWIN_EXTSN'
	'select':    '_select$1050'
	'stat':      '_stat$INODE64'
}

fn darwin_x64_symbols_project() !string {
	root := os.join_path(os.vtmp_dir(), 'darwin_x64_symbols_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'main.v'), 'import os

fn C.fstat(fd i32, buf &C.stat) i32
fn C.fdopendir(fd i32) &C.DIR

fn main() {
	s := os.stat(".") or { panic(err) }
	l := os.lstat(".") or { panic(err) }
	println(s.size + l.size)
	println(os.ls(".") or { [] })
	println(os.real_path("."))
	println(os.fd_is_pending(0))
	mut st := C.stat{}
	println(C.fstat(0, &st))
	println(voidptr(C.fdopendir(-1)) == unsafe { nil })
}
')!
	return root
}

fn generate_darwin_x64_symbols_c(root string, target_os string) ![]string {
	c_source := os.join_path(root, '${target_os}.c')
	result := os.exec([@VEXE, '-new-compiler', '-nocache', '-os', target_os, '-gc', 'none', '-o',
		c_source, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	return os.read_lines(c_source)!
}

fn undefined_symbols(object string) []string {
	result := cmdexec.run('nm', ['-u', object])
	assert result.exit_code == 0, result.output
	return result.output.split_into_lines().map(it.trim_space())
}

fn test_macos_prototypes_link_the_x86_64_sdk_symbols() {
	root := darwin_x64_symbols_project()!
	defer { os.rmdir_all(root) or {} }
	lines := generate_darwin_x64_symbols_c(root, 'macos')!
	assert !lines.any(it.starts_with('#include')), 'macOS output is expected to be headerless'
	for name, symbol in darwin_x64_program_symbols {
		label := ' __asm__("${symbol}");'
		mut idx := -1
		for i, line in lines {
			if line.ends_with(label) && line.contains(' ${name}(') {
				idx = i
				break
			}
		}
		assert idx > 0, 'no x86_64 SDK symbol for `${name}`'
		assert lines[idx - 1] == darwin_x64_guard, name
		assert lines[idx + 1] == '#else', name
		assert lines[idx + 2] == lines[idx].all_before(label) + ';', name
		assert lines[idx + 3] == '#endif', name
	}
	linux_lines := generate_darwin_x64_symbols_c(root, 'linux')!
	assert !linux_lines.any(it.contains('__asm__("_stat$INODE64")'))
	$if !macos {
		return
	}
	// Compile the same C for both macOS architectures with the SDK's compiler and
	// check what it links: Intel gets the 64-bit inode variants, arm64 the plain names.
	for arch in ['x86_64', 'arm64'] {
		object := os.join_path(root, '${arch}.o')
		build := cmdexec.run('cc', ['-arch', arch, '-w', '-c', '-o', object,
			os.join_path(root, 'macos.c')])
		assert build.exit_code == 0, build.output
		symbols := undefined_symbols(object)
		for name, symbol in darwin_x64_program_symbols {
			if arch == 'x86_64' {
				assert symbol in symbols, '${arch}: ${symbols}'
				assert '_${name}' !in symbols, '${arch}: ${symbols}'
			} else {
				assert '_${name}' in symbols, '${arch}: ${symbols}'
				assert !symbols.any(it.contains('$')), '${arch}: ${symbols}'
			}
		}
	}
}

fn test_x86_64_sdk_symbols_are_only_written_for_macos_and_portable_c() {
	mut g := FlatGen.new()
	g.set_target(pref.target_from('macos', 'amd64') or { panic(err) })
	assert (g.c_extern_darwin_x64_symbol('stat') or { '' }) == '_stat$INODE64'
	// arm64 output gets the same text; the C preprocessor picks the plain prototype.
	g.set_target(pref.target_from('macos', 'arm64') or { panic(err) })
	assert (g.c_extern_darwin_x64_symbol('readdir') or { '' }) == '_readdir$INODE64'
	assert g.c_extern_darwin_x64_symbol('closedir') == none
	g.set_target(pref.target_from('linux', 'amd64') or { panic(err) })
	assert g.c_extern_darwin_x64_symbol('stat') == none
	// A portable snapshot such as vc/v.c is compiled on Intel Macs too.
	g.output_cross_c = true
	assert (g.c_extern_darwin_x64_symbol('stat') or { '' }) == '_stat$INODE64'
	g.output_cross_c = false
	g.set_target(pref.target_from('ios', 'arm64') or { panic(err) })
	assert g.c_extern_darwin_x64_symbol('stat') == none
}

// The table is checked against the installed SDK: its headers have to link every
// listed function to the listed symbol on x86_64.
fn test_x86_64_sdk_symbol_table_matches_the_installed_sdk() {
	$if !macos {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'darwin_x64_sdk_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	mut code := ''
	for header in ['dirent.h', 'fts.h', 'ftw.h', 'glob.h', 'stdlib.h', 'syslog.h', 'unistd.h',
		'sys/mount.h', 'sys/select.h', 'sys/stat.h'] {
		code += '#include <${header}>\n'
	}
	code += 'void* v_sdk_symbols[] = {\n'
	for name, _ in c_darwin_x64_sdk_symbols {
		code += '\t(void*)&${name},\n'
	}
	code += '};\n'
	source := os.join_path(dir, 'sdk.c')
	object := os.join_path(dir, 'sdk.o')
	os.write_file(source, code)!
	build := cmdexec.run('cc', ['-arch', 'x86_64', '-w', '-c', '-o', object, source])
	assert build.exit_code == 0, build.output
	symbols := undefined_symbols(object)
	for name, symbol in c_darwin_x64_sdk_symbols {
		assert symbol in symbols, '${name}: ${symbols}'
	}
}
