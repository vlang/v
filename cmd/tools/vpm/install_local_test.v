module main

import os
import rand
import v.vmod
import test_utils { cmd_ok }

const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_local_test_${rand.ulid()}')

struct LocalInstallCase {
	args        string
	module_name string
	workdir     string
}

fn testsuite_begin() {
	test_utils.set_test_env(test_path)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_install_from_local_git_repository_variants() {
	repo_path := os.join_path(test_path, 'local_repo')
	repo_dot_git_path := os.join_path(test_path, 'local_repo.git')
	create_local_git_module(repo_path, 'local_repo_pkg')
	create_local_git_module(repo_dot_git_path, 'local_repo_dot_git_pkg')

	cases := [
		LocalInstallCase{
			args:        os.quoted_path(repo_path)
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        file_url(repo_path)
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        '--git ${os.quoted_path(repo_path)}'
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        '--git ${file_url(repo_path)}'
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        os.file_name(repo_path)
			module_name: 'local_repo_pkg'
			workdir:     test_path
		},
		LocalInstallCase{
			args:        os.quoted_path(repo_dot_git_path)
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        file_url(repo_dot_git_path)
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        '--git ${os.quoted_path(repo_dot_git_path)}'
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        '--git ${file_url(repo_dot_git_path)}'
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        os.file_name(repo_dot_git_path)
			module_name: 'local_repo_dot_git_pkg'
			workdir:     test_path
		},
	]
	for i, c in cases {
		vmodules_path := os.join_path(test_path, 'vmodules_case_${i}')
		test_utils.set_test_env(vmodules_path)
		cmd := '${vexe} install ${c.args}'
		old_dir := os.getwd()
		if c.workdir != '' {
			os.chdir(c.workdir) or { panic(err) }
		}
		res := cmd_ok(@LOCATION, cmd)
		if c.workdir != '' {
			os.chdir(old_dir) or {}
		}
		assert res.output.contains('Installed `${c.module_name}`'), res.output
		manifest := vmod.from_file(os.join_path(vmodules_path, c.module_name, 'v.mod')) or {
			panic('Failed to parse v.mod for `${c.module_name}`. ${err}')
		}
		assert manifest.name == c.module_name
	}
}

fn create_local_git_module(repo_path string, module_name string) {
	os.mkdir_all(repo_path) or { panic(err) }
	os.write_file(os.join_path(repo_path, 'v.mod'), "Module{\n\tname: '${module_name}'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	cmd_ok(@LOCATION, 'git init ${os.quoted_path(repo_path)}')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} add v.mod')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} -c user.email="ci@vlang.io" -c user.name="V CI" commit -m "initial commit"')
}

fn file_url(path string) string {
	mut normalized_path := path.replace('\\', '/')
	if !normalized_path.starts_with('/') {
		normalized_path = '/${normalized_path}'
	}
	return 'file://${normalized_path}'
}
