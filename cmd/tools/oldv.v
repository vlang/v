import os
import flag
import scripting
import vgit

const (
	tool_version     = '0.0.3'
	tool_description = '  Checkout an old V and compile it as it was on specific commit.
|     This tool is useful, when you want to discover when something broke.
|     It is also useful, when you just want to experiment with an older historic V.
|
|     The VCOMMIT argument can be a git commitish like HEAD or master and so on.
|     When oldv is used with git bisect, you probably want to give HEAD. For example:
|          git bisect start
|          git bisect bad
|          git checkout known_good_commit
|          git bisect good
|              ## Now git will automatically checkout a middle commit between the bad and the good
|          cmd/tools/oldv HEAD --command="run commands in oldv folder, to verify if the commit is good or bad"
|              ## See what the result is, and either do: ...
|          git bisect good
|              ## ... or do:
|          git bisect bad
|              ## Now you just repeat the above steps, each time running oldv with the same command, then mark the result as good or bad,
|              ## until you find the commit, where the problem first occurred.
|              ## When you finish, do not forget to do:
|          git bisect reset'.strip_margin()
)

struct Context {
mut:
	vgo      vgit.VGitOptions
	commit_v string = 'master'
	// the commit from which you want to produce a working v compiler (this may be a commit-ish too)
	commit_vc string = 'master'
	// this will be derived from commit_v
	commit_v_hash string // this will be filled from the commit-ish commit_v using rev-list. It IS a commit hash.
	path_v        string // the full path to the v folder inside workdir.
	path_vc       string // the full path to the vc folder inside workdir.
	cmd_to_run    string // the command that you want to run *in* the oldv repo
	cc            string = 'cc'
	// the C compiler to use for bootstrapping.
	cleanup bool // should the tool run a cleanup first
}

fn (mut c Context) compile_oldv_if_needed() {
	mut vgit_context := vgit.VGitContext{
		workdir: c.vgo.workdir
		v_repo_url: c.vgo.v_repo_url
		vc_repo_url: c.vgo.vc_repo_url
		cc: c.cc
		commit_v: c.commit_v
		path_v: c.path_v
		path_vc: c.path_vc
	}
	vgit_context.compile_oldv_if_needed()
	c.commit_v_hash = vgit_context.commit_v__hash
	if !os.exists(vgit_context.vexepath) && c.cmd_to_run.len > 0 {
		// NB: 125 is a special code, that git bisect understands as 'skip this commit'.
		// it is used to inform git bisect that the current commit leads to a build failure.
		exit(125)
	}
}

fn main() {
	scripting.used_tools_must_exist(['git', 'cc'])
	mut context := Context{}
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.file_name(os.executable()))
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('VCOMMIT')
	fp.skip_executable()
	fp.limit_free_args(1, 1)
	context.cleanup = fp.bool('clean', 0, true, 'Clean before running (slower).')
	context.cmd_to_run = fp.string('command', `c`, '', 'Command to run in the old V repo.\n')
	commits := vgit.add_common_tool_options(mut context.vgo, mut fp)
	if commits.len > 0 {
		context.commit_v = commits[0]
	} else {
		context.commit_v = scripting.run('git rev-list -n1 HEAD')
	}
	scripting.cprintln('#################  context.commit_v: $context.commit_v #####################')
	context.path_v = vgit.normalized_workpath_for_commit(context.vgo.workdir, context.commit_v)
	context.path_vc = vgit.normalized_workpath_for_commit(context.vgo.workdir, 'vc')
	if !os.is_dir(context.vgo.workdir) {
		eprintln('Work folder: $context.vgo.workdir , does not exist.')
		exit(2)
	}
	ecc := os.getenv('CC')
	if ecc != '' {
		context.cc = ecc
	}
	if context.cleanup {
		scripting.rmrf(context.path_v)
		scripting.rmrf(context.path_vc)
	}
	context.compile_oldv_if_needed()
	scripting.chdir(context.path_v)
	scripting.cprintln('#     v commit hash: $context.commit_v_hash')
	scripting.cprintln('#   checkout folder: $context.path_v')
	if context.cmd_to_run.len > 0 {
		cmdres := os.execute_or_panic(context.cmd_to_run)
		scripting.cprintln('#           command: ${context.cmd_to_run:-34s} exit code: ${cmdres.exit_code:-4d}  result:')
		println(cmdres.output)
		exit(cmdres.exit_code)
	}
}
