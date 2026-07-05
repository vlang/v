// Regression test for two -usecache $embed_file defects:
//
// 1. RESOLUTION: a relative embed path used to bind to the process CWD first
//    whenever a file existed there, instead of the documented source-relative
//    location ("Paths can be absolute or relative to the source file"). The
//    -usecache `v build-module` child runs chdir-ed to VROOT, so the cached
//    module object could silently embed a different file than the program TU.
//    Here the compiler runs from a CWD where a decoy `../asset.txt` exists —
//    the embed must still bind to the file next to the source.
//
// 2. INVALIDATION: embedded assets are invisible to the .v source hashes, so
//    editing ONLY the embedded file used to keep serving the stale cached
//    module object forever. The .embeds.txt manifest must invalidate it.
import os

const vexe = os.quoted_path(@VEXE)

fn write_asset(proj string, content string) ! {
	os.write_file(os.join_path(proj, 'asset.txt'), content)!
	os.write_file(os.join_path(proj, 'expected.txt'), content)!
}

fn test_usecache_embed_file_source_relative_and_invalidation() {
	$if windows {
		eprintln('skipping on windows (posix shell used to control the build CWD)')
		return
	}
	tmp := os.join_path(os.vtmp_dir(), 'usecache_embed_file_test_${os.getpid()}')
	os.rmdir_all(tmp) or {}
	defer {
		os.rmdir_all(tmp) or {}
	}
	proj := os.join_path(tmp, 'proj')
	os.mkdir_all(os.join_path(proj, 'mymod'))!
	os.write_file(os.join_path(proj, 'v.mod'), "Module {\n\tname: 'proj'\n\tversion: '0.0.1'\n}\n")!
	os.write_file(os.join_path(proj, 'mymod', 'mymod.v'),
		"module mymod\n\npub const src = \$embed_file('../asset.txt').to_string()\n")!
	// the checked program: asserts the EMBEDDED content == the runtime file,
	// so a stale or misresolved embed fails the child test run:
	os.write_file(os.join_path(proj, 'check_test.v'),
		"import os\nimport mymod\n\nfn test_embedded_content_matches_expected() {\n\texpected := os.read_file(os.join_path(os.dir(@FILE), 'expected.txt'))!\n\tassert mymod.src == expected\n}\n")!
	write_asset(proj, 'first-content')!
	// a decoy asset reachable via the SAME relative path from the build CWD:
	decoy_cwd := os.join_path(tmp, 'decoy', 'cwd')
	os.mkdir_all(decoy_cwd)!
	os.write_file(os.join_path(tmp, 'decoy', 'asset.txt'), 'DECOY')!
	// isolated module cache, so the test cannot poison (or be poisoned by)
	// the developer's real one:
	vcache_dir := os.join_path(tmp, 'vcache')
	build_cmd := 'cd ${os.quoted_path(decoy_cwd)} && VCACHE=${os.quoted_path(vcache_dir)} ${vexe} -usecache test ${os.quoted_path(os.join_path(proj,
		'check_test.v'))}'
	//
	// 1. cold build+run from the decoy CWD — must embed the source-relative asset:
	res1 := os.execute(build_cmd)
	assert res1.exit_code == 0, 'cold -usecache run embedded the wrong content (CWD-relative decoy?):\n${res1.output}'
	//
	// 2. edit ONLY the embedded asset (no .v change); the warm cache must
	// invalidate the module object and pick the new content up:
	write_asset(proj, 'second-content')!
	res2 := os.execute(build_cmd)
	assert res2.exit_code == 0, 'warm -usecache run served a stale embedded asset:\n${res2.output}'
}
