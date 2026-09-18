import os
import rand
import test_utils

const vexe = os.quoted_path(@VEXE)
const test_path = os.join_path(os.vtmp_dir(), 'vpm_link_test_${rand.ulid()}')

fn testsuite_begin() {
	test_utils.set_test_env(test_path)
	os.mkdir_all(test_path) or {}
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn execute_in_dir(dir string, cmd string) os.Result {
	old_dir := os.getwd()
	os.chdir(dir) or {
		return os.Result{
			exit_code: -1
			output:    'failed to chdir: ${err}'
		}
	}
	defer {
		os.chdir(old_dir) or {}
	}
	return os.execute(cmd)
}

fn test_link_and_unlink_current_project() {
	module_name := 'author.coollib'
	project_path := os.join_path(test_path, 'project')
	write_vmod(project_path, module_name) or {
		assert false, err.msg()
		return
	}
	project_subdir := os.join_path(project_path, 'src')
	os.mkdir_all(project_subdir) or {
		assert false, err.msg()
		return
	}
	link_path := os.join_path(test_path, 'author', 'coollib')

	link_res := execute_in_dir(project_subdir, '${vexe} link')
	if link_res.exit_code != 0 && is_symlink_privilege_error(link_res.output) {
		eprintln('Skipping symlink test due to missing privileges.')
		return
	}
	assert link_res.exit_code == 0, link_res.output
	assert os.is_link(link_path), 'expected `${link_path}` to be a symlink'
	assert os.real_path(link_path) == os.real_path(project_path)
	assert link_res.output.contains('Linked `${module_name}`'), link_res.output

	link_again_res := execute_in_dir(project_path, '${vexe} link')
	assert link_again_res.exit_code == 0, link_again_res.output
	assert link_again_res.output.contains('already linked')
		|| link_again_res.output.contains('already available'), link_again_res.output

	unlink_res := execute_in_dir(project_subdir, '${vexe} unlink')
	assert unlink_res.exit_code == 0, unlink_res.output
	assert !os.exists(link_path) && !os.is_link(link_path)
	assert !os.exists(os.join_path(test_path, 'author'))
	assert unlink_res.output.contains('Unlinked `${module_name}`'), unlink_res.output
}

// `link` and `unlink` are about the V modules directory. Under `--local` that
// directory is the project itself, where linking would point the project at
// itself and unlinking would take a symlinked module the project keeps of its
// own, so the flag is refused instead of quietly doing either.
fn test_link_and_unlink_reject_the_local_flag() {
	module_name := 'author.locallib'
	project_path := os.join_path(test_path, 'local_flag_project')
	write_vmod(project_path, module_name) or {
		assert false, err.msg()
		return
	}
	// A module of the project's own, symlinked at the path `--local` would have
	// made `unlink` remove.
	own_module := os.join_path(project_path, 'shared_source')
	os.mkdir_all(own_module) or {
		assert false, err.msg()
		return
	}
	linked_own_module := os.join_path(project_path, 'author', 'locallib')
	os.mkdir_all(os.dir(linked_own_module)) or {
		assert false, err.msg()
		return
	}
	os.symlink(own_module, linked_own_module) or {
		eprintln('Skipping symlink test due to missing privileges: ${err}')
		return
	}

	link_res := execute_in_dir(project_path, '${vexe} link --local')
	assert link_res.exit_code == 2, link_res.output
	assert link_res.output.contains('`link` does not accept `--local`'), link_res.output

	unlink_res := execute_in_dir(project_path, '${vexe} unlink --local')
	assert unlink_res.exit_code == 2, unlink_res.output
	assert unlink_res.output.contains('`unlink` does not accept `--local`'), unlink_res.output
	assert os.is_link(linked_own_module), "expected the project's own symlink to survive"
	assert os.real_path(linked_own_module) == os.real_path(own_module)
}

fn test_link_without_vmod() {
	path := os.join_path(test_path, 'no_manifest')
	os.mkdir_all(path) or {
		assert false, err.msg()
		return
	}
	res := execute_in_dir(path, '${vexe} link')
	assert res.exit_code == 1, res.output
	assert res.output.contains('no `v.mod` file found'), res.output
}

fn write_vmod(path string, module_name string) ! {
	os.mkdir_all(path)!
	vmod_path := os.join_path(path, 'v.mod')
	vmod_contents := "Module {\n\tname: '${module_name}'\n\tdescription: ''\n\tversion: '0.0.0'\n\tlicense: 'MIT'\n\tdependencies: []\n}\n"
	os.write_file(vmod_path, vmod_contents)!
}

fn is_symlink_privilege_error(output string) bool {
	lower := output.to_lower()
	return lower.contains('required privilege is not held') || lower.contains('symbolic link')
}
