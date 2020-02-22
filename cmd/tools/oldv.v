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
     When oldv is used with git bisect, you probably want to give HEAD. For example:
          git bisect start
          git bisect bad
          git checkout known_good_commit
          git bisect good
              ## Now git will automatically checkout a middle commit between the bad and the good
          cmd/tools/oldv HEAD --command="run commands in oldv folder, to verify if the commit is good or bad"
              ## See what the result is, and either do: ...
          git bisect good
              ## ... or do:
          git bisect bad
              ## Now you just repeat the above steps, each time running oldv with the same command, then mark the result as good or bad,
              ## until you find the commit, where the problem first occured.
              ## When you finish, do not forget to do:
          git bisect reset'
)

struct Context {
mut:
	v_repo_url    string // the url of the V repository. It can be a local folder path, if you want to eliminate network operations...
	vc_repo_url   string // the url of the vc repository. It can be a local folder path, if you want to eliminate network operations...
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
	mut vgit_context := vgit.VGitContext{
		cc:          c.cc
		workdir:     c.workdir
		commit_v:    c.commit_v
		path_v:      c.path_v
		path_vc:     c.path_vc
		v_repo_url:  c.v_repo_url
		vc_repo_url: c.vc_repo_url
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
	fp.application(filepath.filename(os.executable()))
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('VCOMMIT')
	fp.skip_executable()
	fp.limit_free_args(1, 1)
	
	context.cleanup = fp.bool('clean', true, 'Clean before running (slower).')
	context.cmd_to_run = fp.string_('command', `c`, '', 'Command to run in the old V repo.\n')
	
	commits := vgit.add_common_tool_options(mut context, mut fp)
	if commits.len > 0 {
		context.commit_v = commits[0]
	} else {
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
		scripting.rmrf(context.path_v)
		scripting.rmrf(context.path_vc)
	}
	
	context.compile_oldv_if_needed()
	
	scripting.chdir(context.path_v)
	println('#     v commit hash: $context.commit_v_hash')
	println('#   checkout folder: $context.path_v')
	if context.cmd_to_run.len > 0 {
		cmdres := os.exec(context.cmd_to_run) or {
			panic(err)
		}
		println('#           command: ${context.cmd_to_run:-34s} exit code: ${cmdres.exit_code:-4d}  result:')
		println(cmdres.output)
		exit(cmdres.exit_code)
	}
	
}
