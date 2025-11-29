import os

const vexe = @VEXE
const tfolder = os.to_slash(os.join_path(os.vtmp_dir(), 'fmt_hook_test'))
const unformatted_content = '   fn main() {\nprintln(   "hi" )\n println ( 123 )\n   }'
const formatted_content = "fn main() {\n\tprintln('hi')\n\tprintln(123)\n}\n"
const hook_file = '.git/hooks/pre-commit'
const foreign_script = '#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp\nprintln("hello hello")'

const git = os.to_slash(os.find_abs_path_of_executable('git') or {
	eprintln('git is needed for this test, skipping...')
	exit(0)
})

const v = os.to_slash(os.find_abs_path_of_executable('v') or {
	eprintln('v needs to be installed and available on the path for this test, skipping...')
	exit(0)
})

fn testsuite_begin() {
	unbuffer_stdout()
	eprintln('>>>>>> preparing tfolder: ${tfolder}')
	full_remove(tfolder) or {}
	os.mkdir_all(tfolder) or { panic('> could not create ${tfolder}, err: ${err}') }
	os.chdir(tfolder)!
	os.write_file('main.v', unformatted_content) or { panic(err) }
	assert !os.is_dir('.git')
	os.execute_or_exit('git init .')
	os.execute_or_exit('git config core.eol lf')
	os.execute_or_exit('git config core.autocrlf input')
	os.execute_or_exit('git config user.email "me@example.com"')
	os.execute_or_exit('git config user.name "Myself"')
	assert os.is_dir('.git')
	os.execute_or_exit('git add .')
	os.execute_or_exit('git commit -m "start testing, initially unformatted"')
	os.execute_or_exit('git checkout -b start') // use a known name, instead of master or main or who knows what else ...
	assert read_file('main.v') == unformatted_content
	// show_git_status()	
}

fn testsuite_end() {
	reset_to_start_state()
	show_git_status()
	os.chdir(os.wd_at_startup)!
	full_remove(tfolder)!
	eprintln('>>>>>> deleted ${tfolder}')
	assert true
}

fn test_commit_no_vfmt() {
	eprintln('>>>> ${@FN}')
	reset_to_start_state()
	assert os.execute_or_exit('git checkout -b unformatted').exit_code == 0
	append('main.v', '//') or { panic(err) }
	assert os.execute_or_exit('git add .').exit_code == 0
	assert os.execute_or_exit('git commit -m "unformatted change"').exit_code == 0
	assert os.execute_or_exit('git diff start').exit_code == 0
	assert read_file('main.v').starts_with(unformatted_content)
}

fn test_run_vfmt_manually() {
	eprintln('>>>> ${@FN}')
	reset_to_start_state()
	assert os.execute_or_exit('git checkout -b formatted').exit_code == 0
	os.write_file('README.md', 'some new content') or { panic(err) }
	assert os.execute_or_exit('${os.quoted_path(vexe)} fmt -w .').exit_code == 0
	assert os.execute_or_exit('git add .').exit_code == 0
	assert os.execute_or_exit('git commit -m "formatted change"').exit_code == 0
	assert os.execute_or_exit('git diff start').exit_code == 0
	assert read_file('main.v') == formatted_content
}

fn test_run_git_fmt_hook() {
	eprintln('>>>> ${@FN}')
	reset_to_start_state()
	res := os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook')
	assert res.exit_code == 0
	assert res.output.contains('>   CURRENT git repo pre-commit hook: missing')
	assert res.output.contains('> Main V repo pre-commit hook script: size:  ')
	assert res.output.contains('cmd/tools/git_pre_commit_hook.vsh')
	assert res.output.contains('> Files have different hashes.')
	assert res.output.contains('> Use `v git-fmt-hook install`')
}

fn test_run_git_fmt_hook_status_explicit() {
	eprintln('>>>> ${@FN}')
	reset_to_start_state()
	res := os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook status')
	assert res.exit_code == 0
	assert res.output.contains('>   CURRENT git repo pre-commit hook: missing')
	assert res.output.contains('> Main V repo pre-commit hook script: size:  ')
	assert res.output.contains('cmd/tools/git_pre_commit_hook.vsh')
	assert res.output.contains('> Files have different hashes.')
	assert res.output.contains('> Use `v git-fmt-hook install`')
}

fn test_run_git_fmt_hook_install() {
	eprintln('>>>> ${@FN}')
	reset_to_start_state()
	os.execute_or_exit('git checkout -b formatting_with_hook')
	append('main.v', '\n') or { panic(err) }
	assert read_file('main.v').starts_with(unformatted_content)
	assert !os.is_file(hook_file)
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook install').exit_code == 0
	assert os.is_file(hook_file)
	res := os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook status')
	assert res.output.contains('>   CURRENT git repo pre-commit hook: size:  ')
	assert res.output.contains('> Main V repo pre-commit hook script: size:  ')
	assert res.output.contains('cmd/tools/git_pre_commit_hook.vsh')
	assert res.output.contains(hook_file)
	assert res.output.contains('> Both files are exactly the same.')
	assert !res.output.contains('> Use `v git-fmt-hook install`')
	assert res.output.contains('> Use `v git-fmt-hook remove`')
	assert !res.output.contains('> Done.'), 'res:\n${res}'
	os.execute_or_exit('git add -u')
	os.execute_or_exit('git commit -m "this should be formatted"')
	assert read_file('main.v') == formatted_content
	dres := os.execute_or_exit('git diff start')
	// dump(dres)
	assert dres.exit_code == 0
	assert dres.output.contains('+fn main() {')
	assert dres.output.contains("+\tprintln('hi')")
	second := os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook install')
	assert second.exit_code == 0
	assert second.output.contains('> Done.'), 'second:\n${second}'
}

fn test_run_git_fmt_hook_remove() {
	eprintln('>>>> ${@FN}')
	reset_to_start_state()
	os.execute_or_exit('git checkout start')
	os.execute_or_exit('git checkout -b non_formatting_after_removing_hook')
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook install').exit_code == 0
	assert os.is_file(hook_file)
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook remove').exit_code == 0
	assert !os.is_file(hook_file)
	append('main.v', '\n') or { panic(err) }
	assert read_file('main.v').starts_with(unformatted_content)
	os.execute_or_exit('git add -u')
	os.execute_or_exit('git commit -m "this should NOT be formatted again"')
	assert read_file('main.v').starts_with(unformatted_content)
}

fn test_run_git_fmt_hook_install_and_remove_on_foreign_hook_should_be_a_nop() {
	eprintln('>>>> ${@FN}')
	reset_to_start_state()
	os.execute_or_exit('git checkout start')
	os.execute_or_exit('git checkout -b install_and_remove_should_be_a_nop_on_a_foreign_hook')
	os.write_file(hook_file, foreign_script) or { panic(err) }
	os.chmod(hook_file, 0o0777) or { panic(err) }
	assert read_file(hook_file) == foreign_script
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook install').exit_code == 0
	assert read_file(hook_file) == foreign_script
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook remove').exit_code == 0
	assert read_file(hook_file) == foreign_script
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook install').exit_code == 0
	assert read_file(hook_file) == foreign_script
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook status').exit_code == 0
	assert read_file(hook_file) == foreign_script
	assert os.execute_or_exit('${os.quoted_path(vexe)} git-fmt-hook').exit_code == 0
	assert read_file(hook_file) == foreign_script
	append('main.v', '\n') or { panic(err) }
	append('main.v', '\n') or { panic(err) }
	assert read_file('main.v').starts_with(unformatted_content)
	os.execute_or_exit('git add -u')
	fcommiting := os.execute_or_exit('git commit -m "this should NOT be formatted 2"')
	assert fcommiting.exit_code == 0
	assert fcommiting.output.contains('hello hello')
	assert read_file('main.v').starts_with(unformatted_content)
}

fn show_git_status() {
	os.system('git log --graph --all --decorate')
	os.system('git status')
}

fn append(path string, content string) ! {
	mut f := os.open_append('main.v')!
	f.write_string(content)!
	f.close()
}

fn read_file(path string) string {
	return os.read_file(path) or { panic(err) }
}

fn reset_to_start_state() {
	os.execute('git checkout start')
	os.rm('.git/hooks/pre-commit') or {}
	assert read_file('main.v') == unformatted_content
}

fn full_remove(path string) ! {
	// TODO: fix this on windows; the files inside .git/ are with read only permissions, and os.rmdir_all() can not delete them, until they are chmoded to writable
	files := os.walk_ext(path + '/.git', '')
	for f in files {
		os.chmod(f, 0o777) or {}
	}
	os.rmdir_all(path)!
}
