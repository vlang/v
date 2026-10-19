module driver

import os
import time
import v.cmdexec
import v.gen.fastc
import v.pref

const fastc_entry_source = "fn main() {\n\tprintln('fastc entry point')\n}\n"

fn fastc_entry_c_source(target_os string) string {
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.target = pref.target_from(target_os, 'amd64') or { panic(err) }
	return fastc.generate(fastc_entry_source, 'fastc_entry.v', prefs) or { panic(err) }
}

// Windows executables link with `-municode`, whose CRT startup calls `wmain`, so the
// fastc entry has to be `wmain` there (as the C backend's is); elsewhere no such flag
// is passed and the entry stays `main`.
fn test_fastc_entry_point_agrees_with_windows_linker_flags() {
	for c_compiler in ['gcc', 'tinyc', 'clang'] {
		assert '-municode' in v3_windows_executable_linker_flags('windows', c_compiler, false,
			false, .auto, false), c_compiler
	}
	windows_source := fastc_entry_c_source('windows')
	assert windows_source.contains('int wmain(void) {'), windows_source
	assert !windows_source.contains('int main('), windows_source

	assert v3_windows_executable_linker_flags('linux', 'gcc', false, false, .auto, false) == []
	linux_source := fastc_entry_c_source('linux')
	assert linux_source.contains('int main(void) {'), linux_source
	assert !linux_source.contains('wmain'), linux_source
}

fn test_fastc_windows_entry_point_links_with_windows_linker_flags() {
	$if !windows {
		return
	}
	mut compilers := [][]string{}
	bundled_tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if os.exists(bundled_tcc) {
		compilers << ['tinyc', bundled_tcc]
	}
	if gcc := os.find_abs_path_of_executable('gcc') {
		compilers << ['gcc', gcc]
	}
	if compilers.len == 0 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_fastc_windows_entry_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		// A just-run executable can still be held open briefly (antivirus scans).
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'entry.c')
	os.write_file(c_file, fastc_entry_c_source('windows'))!
	for compiler in compilers {
		output := os.join_path(root, 'entry_${compiler[0]}.exe')
		mut args := ['-std=gnu11']
		args << v3_windows_executable_linker_flags('windows', compiler[0], false, false, .auto,
			false)
		args << ['-o', output, c_file]
		compiled := cmdexec.run(compiler[1], args)
		assert compiled.exit_code == 0, '${compiler[1]} ${args.join(' ')}\n${compiled.output}'
		ran := cmdexec.run(output, [])
		assert ran.exit_code == 0, ran.output
		assert ran.output.contains('fastc entry point'), ran.output
	}
}
