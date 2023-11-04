import os
import v.vmod

const (
	v         = os.quoted_path(@VEXE)
	// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
	// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test-vmodules/`.
	test_path = os.join_path(os.vtmp_dir(), 'test-vmodules')
)

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_install_from_vpm_ident() {
	res := os.execute('${v} install nedpals.args')
	assert res.exit_code == 0, res.output
	mod := vmod.from_file(os.join_path(test_path, 'nedpals', 'args', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'nedpals.args'
	assert mod.dependencies == []string{}
}

fn test_install_from_vpm_short_ident() {
	res := os.execute('${v} install pcre')
	assert res.exit_code == 0, res.output
	mod := vmod.from_file(os.join_path(test_path, 'pcre', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'pcre'
	assert mod.description == 'A simple regex library for V.'
}

fn test_install_from_git_url() {
	res := os.execute('${v} install https://github.com/vlang/markdown')
	assert res.exit_code == 0, res.output
	assert res.output.contains('Installing module "markdown" from "https://github.com/vlang/markdown')
	assert res.output.contains('Relocating module from "vlang/markdown" to "markdown"')
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
}

fn test_install_already_existent() {
	mut res := os.execute('${v} install https://github.com/vlang/markdown')
	assert res.exit_code == 0, res.output
	assert res.output.contains('already exists')
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
}

fn test_install_once() {
	// Start with a clean test path.
	$if windows {
		// FIXME: Workaround for failing `rmdir` commands on Windows.
		os.system('rd /s /q ${test_path}')
	} $else {
		os.rmdir_all(test_path) or {}
	}
	os.mkdir_all(test_path) or {}

	// Install markdown module.
	mut res := os.execute('${v} install markdown')
	assert res.exit_code == 0, res.output
	// Keep track of the last modified state of the v.mod file of the installed markdown module.
	md_last_modified := os.file_last_mod_unix(os.join_path(test_path, 'markdown', 'v.mod'))

	install_cmd := '${@VEXE} install https://github.com/vlang/markdown https://github.com/vlang/pcre --once -v'
	// Try installing two modules, one of which is already installed.
	res = os.execute(install_cmd)
	assert res.exit_code == 0, res.output
	assert res.output.contains("Already installed modules: ['markdown']")
	mod := vmod.from_file(os.join_path(test_path, 'pcre', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'pcre'
	assert mod.description == 'A simple regex library for V.'
	// Ensure the before installed markdown module wasn't modified.
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))

	// Try installing two modules that are both already installed.
	res = os.execute(install_cmd)
	assert res.exit_code == 0, res.output
	assert res.output.contains('All modules are already installed.')
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))
}

fn test_missing_repo_name_in_url() {
	incomplete_url := 'https://github.com/vlang'
	res := os.execute('${v} install ${incomplete_url}')
	assert res.exit_code == 1
	assert res.output.trim_space() == 'Errors while retrieving module name for: "${incomplete_url}"'
}
