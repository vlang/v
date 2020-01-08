import (
	os
	flag
	filepath
	scripting
	vgit
)

const (
	tool_version = '0.0.3'
	tool_description = '  Checkout an old V and compile it as it was on specific commit.
     This tool is useful, when you want to discover when something broke.
     It is also useful, when you just want to experiment with an older historic V.

     The VCOMMIT argument can be a git commitish like HEAD or master and so on.
     When oldv is used with git bisect, you probably want to use HEAD. For example:
          git bisect start
          git bisect bad
          git checkout known_good_commit
          git bisect good
              ## Now git will automatically checkout a middle commit between the bad and the good
          tools/oldv HEAD --command="run commands in oldv folder, to verify if the commit is good or bad"
              ## See what the result is, and either do: ...
          git bisect good
              ## ... or do:
          git bisect bad
              ## Now you just repeat the above steps, each time running oldv with the same command, then mark the result as good or bad,
              ## until you find the commit, where the problem first occured.
              ## When you finish, do not forget to do:
          git bisect reset
'
	remote_repo_url_v = 'https://github.com/vlang/v'
	remote_repo_url_vc = 'https://github.com/vlang/vc'
)

struct Context {
mut:
	repo_url_v    string // the url of the V repository. It can be a local folder path, if you want to eliminate network operations...
	repo_url_vc   string // the url of the vc repository. It can be a local folder path, if you want to eliminate network operations...
	workdir       string // the working folder (typically /tmp), where the tool will write
	commit_v      string='master' // the commit from which you want to produce a working v compiler (this may be a commit-ish too)
	commit_vc     string='master' // this will be derived from commit_v
	commit_v_hash string // this will be filled from the commit-ish commit_v using rev-list. It IS a commit hash.
	path_v        string // the full path to the v folder inside workdir.
	path_vc       string // the full path to the vc folder inside workdir.
	cmd_to_run    string // the command that you want to run *in* the oldv repo
	cc            string='cc' // the C compiler to use for bootstrapping.
	cleanup       bool // should the tool run a cleanup first
	verbose       bool // should the tool be much more verbose
	show_help     bool // whether to show the usage screen
}

fn (c mut Context) compile_oldv_if_needed() {
	vexename := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	vexepath := filepath.join(c.path_v,vexename)
	mut command_for_building_v_from_c_source := ''
	mut command_for_selfbuilding := ''
	if 'windows' == os.user_os() {
		command_for_building_v_from_c_source = '$c.cc -std=c99 -municode -w -o cv.exe  "$c.path_vc/v_win.c" '
		command_for_selfbuilding = './cv.exe -o $vexename {SOURCE}'
	}
	else {
		command_for_building_v_from_c_source = '$c.cc -std=gnu11 -w -o cv "$c.path_vc/v.c"  -lm'
		command_for_selfbuilding = './cv -o $vexename {SOURCE}'
	}
	scripting.chdir(c.workdir)
	scripting.run('git clone --quiet "$c.repo_url_v"   "$c.path_v" ')
	scripting.run('git clone --quiet "$c.repo_url_vc"  "$c.path_vc" ')
	scripting.chdir(c.path_v)
	scripting.run('git checkout $c.commit_v')
	v_commithash,vccommit_before := vgit.prepare_vc_source(c.path_vc, c.path_v, c.commit_v)
	c.commit_v_hash = v_commithash
	c.commit_vc = vccommit_before
	if os.is_dir(c.path_v) && os.exists(vexepath) {
		// already compiled, so no need to compile v again
		return
	}
	scripting.run('git clean -xf')
	scripting.run(command_for_building_v_from_c_source)
	source_location := if os.exists('v.v') { 'v.v' } else { 'compiler' }
	build_cmd := command_for_selfbuilding.replace('{SOURCE}', source_location)
	scripting.run(build_cmd)
	if !os.exists(vexepath) && c.cmd_to_run.len > 0 {
		// NB: 125 is a special code, that git bisect understands as 'skip this commit'.
		// it is used to inform git bisect that the current commit leads to a build failure.
		exit(125)
	}
}

fn main() {
	scripting.used_tools_must_exist(['git', 'cc'])
	mut context := Context{}
	mut fp := flag.new_flag_parser(os.args)
	fp.application(filepath.filename(os.executable()))
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('VCOMMIT')
	fp.skip_executable()
	fp.limit_free_args(1, 1)
	context.show_help = fp.bool_('help', `h`, false, 'Show this help screen.')
	context.verbose = fp.bool_('verbose', `v`, false, 'Be more verbose.\n')
	context.cleanup = fp.bool('clean', true, 'Clean before running (slower).')
	context.cmd_to_run = fp.string_('command', `c`, '', 'Command to run in the old V repo.')
	context.workdir = os.realpath(fp.string_('work-dir', `w`, os.tmpdir(), 'A writable folder, where the comparison will be done.\n'))
	context.repo_url_v = fp.string('v-repo', remote_repo_url_v, 'The url of the V repository. You can clone it locally too.\n')
	context.repo_url_vc = fp.string('vc-repo', remote_repo_url_vc, '' + 'The url of the vc repository. You can clone it \n' + flag.SPACE + 'beforehand, and then just give the local folder \n' + flag.SPACE + 'path here. That will eliminate the network ops  \n' + flag.SPACE + 'done by this tool, which is useful, if you want \n' + flag.SPACE + 'to script it/run it in a restrictive vps/docker.')
	if (context.show_help) {
		println(fp.usage())
		exit(0)
	}
	if context.verbose {
		scripting.set_verbose(true)
	}
	commits := fp.finalize() or {
		eprintln('Error: ' + err)
		exit(1)
	}
	if commits.len > 0 {
		context.commit_v = commits[0]
		vgit.validate_commit_exists(context.commit_v)
	}
	else {
		context.commit_v = scripting.run('git rev-list -n1 HEAD')
	}
	println('#################  context.commit_v: $context.commit_v #####################')
	context.path_v = vgit.normalized_workpath_for_commit(context.workdir, context.commit_v)
	context.path_vc = vgit.normalized_workpath_for_commit(context.workdir, 'vc')
	if !os.is_dir(context.workdir) {
		msg := 'Work folder: ' + context.workdir + ' , does not exist.'
		eprintln(msg)
		exit(2)
	}
	ecc := os.getenv('CC')
	if ecc != '' {
		context.cc = ecc
	}
	if context.cleanup {
		scripting.run('rm -rf $context.path_v')
		scripting.run('rm -rf $context.path_vc')
	}
	context.compile_oldv_if_needed()
	scripting.chdir(context.path_v)
	println('#     v commit hash: $context.commit_v_hash')
	println('#   checkout folder: $context.path_v')
	if context.cmd_to_run.len > 0 {
		cmdres := os.exec(context.cmd_to_run) or {
			panic(err)
		}
		println('#           command: $context.cmd_to_run')
		println('# command exit code: $cmdres.exit_code')
		println('# command result   :')
		println(cmdres.output)
		exit(cmdres.exit_code)
	}
}
