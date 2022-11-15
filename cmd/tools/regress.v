import os
import term
import flag

const tools_folder = os.real_path(os.dir(os.executable()))

const oldvexe = fullpath(tools_folder, 'oldv')

const oldv_source = fullpath(tools_folder, 'oldv.v')

const vroot = os.real_path(os.dir(os.dir(tools_folder)))

const vexe = fullpath(vroot, 'v')

fn fullpath(folder string, fname string) string {
	return os.real_path(os.join_path_single(folder, exename(fname)))
}

fn exename(n string) string {
	if n.ends_with('.v') || os.user_os() != 'windows' {
		return n
	}
	return '${n}.exe'
}

struct Context {
mut:
	old_commit string
	new_commit string
	command    string
}

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	mut context := Context{}
	fp.application(os.file_name(os.executable()))
	fp.version('0.0.2')
	fp.description('\n  Find at what commit a regression occurred.
  To find when a regression happened (regression_bug.v should fail on master):
     ./v run cmd/tools/regress.v --old a7019ac --command "  ./v run /abs/path/to/regression_bug.v"
  To find when a feature was implemented (feature.v should succeed on master):
     ./v run cmd/tools/regress.v --old a7019ac --command "! ./v run /abs/path/to/feature.v"')
	fp.skip_executable()
	//
	context.new_commit = fp.string('new', `n`, 'master', 'The new commit, by default: master.')
	context.old_commit = fp.string('old', `o`, '', 'A known old commit, required (for it, COMMAND should exit with 0).')
	context.command = fp.string('command', `c`, '', 'A command to execute. Should exit with 0 for the *old* commits.')
	fp.finalize() or {}
	if context.old_commit == '' {
		eprintln('--old COMMIT is required')
		exit(1)
	}
	if context.command == '' {
		eprintln('--command "COMMAND" is required')
		exit(2)
	}
	if !os.exists(oldvexe) {
		if 0 != execute('${os.quoted_path(vexe)} -o ${os.quoted_path(oldvexe)} ${os.quoted_path(oldv_source)}') {
			panic('can not compile ${oldvexe}')
		}
	}
	os.execute('git checkout master')
	os.execute('git bisect reset')
	os.execute('git checkout ${context.new_commit}')
	os.execute('git bisect start')
	os.execute('git bisect new')
	os.execute('git checkout ${context.old_commit}')
	os.execute('git bisect old')
	println(term.colorize(term.bright_yellow, term.header('', '-')))
	execute('git bisect run ${os.quoted_path(oldvexe)} --bisect -c "${context.command}"')
	println(term.colorize(term.bright_yellow, term.header('', '-')))
	os.execute('git bisect reset')
	os.execute('git checkout master')
}

fn execute(cmd string) int {
	eprintln('### ${cmd}')
	return os.system(cmd)
}
