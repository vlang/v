// vtest build: !windows
// Regression test for https://github.com/vlang/v/issues/28061
// `v new <name>` / `v init` used to block on interactive prompts even when stdin
// is not a terminal, so piping/redirecting stdin made them appear to hang. They
// must instead fall back to defaults for the metadata prompts.
import os
import v.vmod

const vexe = @VEXE

fn test_new_non_interactive_uses_defaults() {
	tdir := os.join_path(os.vtmp_dir(), 'vcreate_noninteractive_${os.getpid()}')
	os.rmdir_all(tdir) or {}
	os.mkdir_all(tdir) or { panic(err) }
	defer {
		os.rmdir_all(tdir) or {}
	}
	old_wd := os.getwd()
	os.chdir(tdir) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
	}
	name := 'my_ni_project'
	// `< /dev/null` guarantees a non-terminal stdin that returns EOF immediately.
	res := os.execute('${os.quoted_path(vexe)} new ${name} < /dev/null')
	assert res.exit_code == 0, res.output
	mod := vmod.from_file(os.join_path(tdir, name, 'v.mod')) or {
		assert false, err.str()
		return
	}
	assert mod.name == name
	assert mod.description == ''
	assert mod.version == '0.0.0'
	assert mod.license == 'MIT'
}
