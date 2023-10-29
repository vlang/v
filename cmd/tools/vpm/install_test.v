import os
import v.vmod

// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test-vmodules/`.
const test_path = os.join_path(os.vtmp_dir(), 'test-vmodules')

fn prep_test_path() {
	os.setenv('VMODULES', test_path, true)
}

fn test_install_from_git_url() {
	prep_test_path()
	res := os.execute(@VEXE + ' install https://github.com/vlang/markdown')
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
}

fn test_install_from_vpm_ident() {
	prep_test_path()
	res := os.execute(@VEXE + ' install nedpals.args')
	mod := vmod.from_file(os.join_path(test_path, 'nedpals', 'args', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'nedpals.args'
	assert mod.dependencies == []string{}
}

fn test_install_from_vpm_short_ident() {
	prep_test_path()
	res := os.execute(@VEXE + ' install pcre')
	mod := vmod.from_file(os.join_path(test_path, 'pcre', 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == 'pcre'
	assert mod.description == 'A simple regex library for V.'
}
