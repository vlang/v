import os

fn test_check_md_uses_vexe_from_environment() ! {
	root := os.join_path(os.vtmp_dir(), 'vcheck_md_vexe_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}

	tool := os.join_path(root, 'vcheck-md')
	$if windows {
		tool += '.exe'
	}
	build := os.exec([@VEXE, '-o', tool, os.join_path(@VEXEROOT, 'cmd', 'tools',
		'vcheck-md.v')])
	assert build.exit_code == 0, build.output

	marker := os.join_path(root, 'used')
	fake := os.join_path(root, 'fake-v')
	$if windows {
		fake += '.bat'
		os.write_file(fake, '@echo off\r\necho ok > ${os.quoted_path(marker)}\r\nexit /b 0\r\n')!
	} $else {
		os.write_file(fake, '#!/bin/sh\nprintf ok > ${os.quoted_path(marker)}\nexit 0\n')!
		os.chmod(fake, 0o755)!
	}

	fixture := os.join_path(root, 'test.md')
	os.write_file(fixture, '```v\nfn main() {}\n```\n')!

	old_vexe := os.getenv_opt('VEXE') or { '' }
	defer {
		if old_vexe == '' {
			os.unsetenv('VEXE')
		} else {
			os.setenv('VEXE', old_vexe, true)
		}
	}
	os.setenv('VEXE', fake, true)

	res := os.exec([tool, 'check-md', fixture])
	assert res.exit_code == 0, res.output
	assert os.exists(marker), 'check-md did not use VEXE from the environment'
}
