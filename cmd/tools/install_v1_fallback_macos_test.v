import crypto.sha256
import os
import v.cmdexec

fn test_v1_release_installer_preserves_absolute_library_symlinks() {
	zip := os.find_abs_path_of_executable('zip') or { return }
	os.find_abs_path_of_executable('bsdtar') or { return }
	os.find_abs_path_of_executable('curl') or { return }
	dir := os.join_path(os.vtmp_dir(), 'v1_installer_library_link_${os.getpid()}')
	os.mkdir_all(dir)!
	os.chmod(dir, 0o700)!
	defer {
		os.rmdir_all(dir) or {}
	}
	release := os.join_path(dir, 'v')
	for path in ['thirdparty/tcc/lib', 'vlib/crypto/internal/subtle', 'vlib/x/json2'] {
		os.mkdir_all(os.join_path(release, path))!
	}
	os.write_file(os.join_path(release, 'v'), '#!/bin/sh\necho "V 0.5.2 test-release"\n')!
	os.chmod(os.join_path(release, 'v'), 0o755)!
	for path in ['vlib/crypto/internal/subtle/aliasing.v', 'vlib/crypto/internal/subtle/comparison.v',
		'vlib/x/json2/json2.v'] {
		os.write_file(os.join_path(release, path), '// release module\n')!
	}
	// This mirrors the absolute library link in the official macOS archive.
	library := os.join_path(dir, 'system-library.dylib')
	os.write_file(library, 'library fixture')!
	os.symlink(library, os.join_path(release, 'thirdparty/tcc/lib/libc.dylib'))!
	archive := os.join_path(dir, 'release.zip')
	packed := cmdexec.run_in(zip, ['-q', '-y', '-r', archive, 'v'], dir)
	assert packed.exit_code == 0, packed.output
	bootstrap := os.join_path(dir, 'unexpected-bootstrap')
	os.write_file(bootstrap, '#!/bin/sh\necho "unexpected source fallback" >&2\nexit 1\n')!
	os.chmod(bootstrap, 0o755)!
	cache := os.join_path(dir, 'cache')
	output := os.join_path(dir, 'installed', 'v1_fallback')
	installer := os.join_path(@VEXEROOT, 'cmd', 'tools', 'install_v1_fallback.sh')
	installed := cmdexec.run_in_merged_with_env('/bin/sh', [installer, bootstrap, output], @VEXEROOT,
		{
			'TMPDIR':                     dir
			'V1_FALLBACK_CACHE_DIR':      cache
			'V1_FALLBACK_RELEASE_URL':    'file://${archive}'
			'V1_FALLBACK_RELEASE_SHA256': sha256.sum(os.read_bytes(archive)!).hex()
			'V1_FALLBACK_BOOTSTRAP':      bootstrap
			'V1_FALLBACK_OUTPUT':         output
		})
	assert installed.exit_code == 0, installed.output
	assert installed.output.contains('Installed the V 0.5.2 release fallback'), installed.output
	assert !installed.output.contains('Building the V 0.5.2 fallback'), installed.output
	assert os.read_file(output + '.vroot')!.trim_space() == os.join_path(cache, '0.5.2')
	link := os.join_path(cache, '0.5.2', 'thirdparty/tcc/lib/libc.dylib')
	assert os.is_link(link)
	assert os.real_path(link) == os.real_path(library)
	assert os.read_file(link)! == 'library fixture'
	assert os.is_file(os.join_path(cache, '0.5.2', 'vlib/crypto/subtle/aliasing.v'))
	assert os.is_file(os.join_path(cache, '0.5.2', 'vlib/json2/.v1-fallback-complete'))
	run := cmdexec.run(output, ['version'])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'V 0.5.2 test-release', run.output
}
