import os

// A program that imports `net` must build with the bundled TCC, without
// falling back to another C compiler. A header or an import library that TCC
// does not ship, used by a file of `net` itself, breaks every such program on
// a machine that has no gcc (https://github.com/vlang/v/issues/33014). Where
// gcc is installed the fallback succeeds and only its warning shows, so the
// warning is what this test refuses.
fn test_import_net_builds_with_the_bundled_tcc_without_a_fallback() {
	vexe := @VEXE
	tcc := os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.exists(tcc) {
		eprintln('skipping: no bundled tcc at ${tcc}')
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'import_net_with_tcc_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'import net\n\nfn main() {\n\tprintln(net.validate_port(80) or { 0 })\n}\n')!
	exe := os.join_path(dir, 'main.exe')
	build := os.exec([vexe, '-cc', 'tcc', '-o', exe, source])
	assert !build.output.contains('falling back'), build.output
	assert !build.output.contains('could not be used'), build.output
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '80'
}
