module pref

import os
import v.cmdexec

fn test_detect_vroot_follows_the_installed_executable_before_its_build_checkout() {
	root := os.join_path(os.vtmp_dir(), 'v_pref_relocation_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	exe_ext := $if windows { '.exe' } $else { '' }
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(@VMODROOT, 'v${exe_ext}')
	} else {
		@VEXE
	}
	// VTMP can itself live in a checkout. Put the standalone executable beyond
	// that checkout's bounded ancestor search so this case exercises the fallback.
	mut standalone_dir := root
	for _ in 0 .. 16 {
		if detect_vroot_from(standalone_dir) == '' {
			break
		}
		standalone_dir = os.join_path(standalone_dir, 's')
		os.mkdir_all(standalone_dir)!
	}
	assert detect_vroot_from(standalone_dir) == ''
	source := os.join_path(root, 'probe.v')
	standalone := os.join_path(standalone_dir, 'probe${exe_ext}')
	os.write_file(source, 'import v.pref\nfn main() { println(pref.new_preferences().vroot) }\n')!
	build := cmdexec.run(vexe, ['-new-compiler', '-gc', 'none', '-o', standalone, source])
	assert build.exit_code == 0, build.output
	outside := os.join_path(root, 'unrelated project')
	os.mkdir_all(outside)!
	// A standalone tool still needs the checkout baked into the imported pref module.
	initial := cmdexec.run_in(standalone, [], outside)
	assert initial.exit_code == 0, initial.output
	baked_root := initial.output.trim_space()
	assert baked_root == os.real_path(@VMODROOT), initial.output
	assert os.is_dir(os.join_path(baked_root, 'vlib', 'builtin'))

	// Both the build checkout and an enclosing checkout remain present. The compiler
	// must select the nearest checkout around its installed executable, even when
	// launched from an unrelated project's directory.
	workspace := os.join_path(root, 'workspace')
	os.mkdir_all(os.join_path(workspace, 'vlib', 'builtin'))!
	for checkout in [os.join_path(root, 'relocated'), os.join_path(workspace, 'vlang')] {
		os.mkdir_all(os.join_path(checkout, 'vlib', 'builtin'))!
		installed := os.join_path(checkout, 'v${exe_ext}')
		os.cp(standalone, installed)!
		os.chmod(installed, 0o755)!
		result := cmdexec.run_in(installed, [], outside)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == os.real_path(checkout), result.output
	}
	$if !windows {
		// Resolve the executable itself, rather than the path of a symlink used to
		// invoke it. The link's directory is deliberately not a V checkout.
		linked := os.join_path(outside, 'v-link')
		nested_checkout := os.join_path(workspace, 'vlang')
		os.symlink(os.join_path(nested_checkout, 'v'), linked)!
		result := cmdexec.run_in(linked, [], outside)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == os.real_path(nested_checkout), result.output
	}
}
