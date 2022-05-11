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
|          cmd/tools/oldv --bisect --command="run commands in oldv folder, to verify if the commit is good or bad"
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
	vgo           vgit.VGitOptions
	vgcontext     vgit.VGitContext
	commit_v      string = 'master' // the commit from which you want to produce a working v compiler (this may be a commit-ish too)
	commit_v_hash string // this will be filled from the commit-ish commit_v using rev-list. It IS a commit hash.
	path_v        string // the full path to the v folder inside workdir.
	path_vc       string // the full path to the vc folder inside workdir.
	cmd_to_run    string // the command that you want to run *in* the oldv repo
	cc            string = 'cc' // the C compiler to use for bootstrapping.
	cleanup       bool   // should the tool run a cleanup first
	use_cache     bool   // use local cached copies for --vrepo and --vcrepo in
	fresh_tcc     bool   // do use `make fresh_tcc`
	is_bisect     bool   // bisect mode; usage: `cmd/tools/oldv -b -c './v run bug.v'`
}

fn (mut c Context) compile_oldv_if_needed() {
	c.vgcontext = vgit.VGitContext{
		workdir: c.vgo.workdir
		v_repo_url: c.vgo.v_repo_url
		vc_repo_url: c.vgo.vc_repo_url
		cc: c.cc
		commit_v: c.commit_v
		path_v: c.path_v
		path_vc: c.path_vc
		make_fresh_tcc: c.fresh_tcc
	}
	c.vgcontext.compile_oldv_if_needed()
	c.commit_v_hash = c.vgcontext.commit_v__hash
	if !os.exists(c.vgcontext.vexepath) && c.cmd_to_run.len > 0 {
		// Note: 125 is a special code, that git bisect understands as 'skip this commit'.
		// it is used to inform git bisect that the current commit leads to a build failure.
		exit(125)
	}
}

const cache_oldv_folder = os.join_path(os.cache_dir(), 'oldv')

const cache_oldv_folder_v = os.join_path(cache_oldv_folder, 'v')

const cache_oldv_folder_vc = os.join_path(cache_oldv_folder, 'vc')

fn sync_cache() {
	scripting.verbose_trace(@FN, 'start')
	if !os.exists(cache_oldv_folder) {
		scripting.verbose_trace(@FN, 'creating $cache_oldv_folder')
		scripting.mkdir_all(cache_oldv_folder) or {
			scripting.verbose_trace(@FN, '## failed.')
			exit(1)
		}
	}
	scripting.chdir(cache_oldv_folder)
	for reponame in ['v', 'vc'] {
		repofolder := os.join_path(cache_oldv_folder, reponame)
		if !os.exists(repofolder) {
			scripting.verbose_trace(@FN, 'cloning to $repofolder')
			scripting.exec('git clone --quiet https://github.com/vlang/$reponame $repofolder') or {
				scripting.verbose_trace(@FN, '## error during clone: $err')
				exit(1)
			}
		}
		scripting.chdir(repofolder)
		scripting.exec('git pull --quiet') or {
			scripting.verbose_trace(@FN, 'pulling to $repofolder')
			scripting.verbose_trace(@FN, '## error during pull: $err')
			exit(1)
		}
	}
	scripting.verbose_trace(@FN, 'done')
}

fn main() {
	scripting.used_tools_must_exist(['git', 'cc'])
	//
	// Resetting VEXE here allows for `v run cmd/tools/oldv.v'.
	// the parent V would have set VEXE, which later will
	// affect the V's run from the tool itself.
	os.setenv('VEXE', '', true)
	//
	mut context := Context{}
	context.vgo.workdir = cache_oldv_folder
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.file_name(os.executable()))
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('VCOMMIT')
	fp.skip_executable()
	context.use_cache = fp.bool('cache', `u`, true, 'Use a cache of local repositories for --vrepo and --vcrepo in \$HOME/.cache/oldv/')
	if context.use_cache {
		context.vgo.v_repo_url = cache_oldv_folder_v
		context.vgo.vc_repo_url = cache_oldv_folder_vc
	} else {
		context.vgo.v_repo_url = 'https://github.com/vlang/v'
		context.vgo.vc_repo_url = 'https://github.com/vlang/vc'
	}
	should_sync := fp.bool('cache-sync', `s`, false, 'Update the local cache')
	context.is_bisect = fp.bool('bisect', `b`, false, 'Bisect mode. Use the current commit in the repo where oldv is.')
	if !should_sync && !context.is_bisect {
		fp.limit_free_args(1, 1)?
	}
	////
	context.cleanup = fp.bool('clean', 0, false, 'Clean before running (slower).')
	context.fresh_tcc = fp.bool('fresh_tcc', 0, true, 'Do `make fresh_tcc` when preparing a V compiler.')
	context.cmd_to_run = fp.string('command', `c`, '', 'Command to run in the old V repo.\n')
	commits := vgit.add_common_tool_options(mut context.vgo, mut fp)
	if should_sync {
		sync_cache()
		exit(0)
	}
	if context.use_cache {
		if !os.is_dir(cache_oldv_folder_v) || !os.is_dir(cache_oldv_folder_vc) {
			sync_cache()
		}
	}
	if commits.len > 0 {
		context.commit_v = commits[0]
		if context.is_bisect {
			eprintln('In bisect mode, you should not pass any commits, since oldv will use the current one.')
			exit(2)
		}
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
	shorter_hash := context.commit_v_hash[0..10]
	scripting.cprintln('#     v commit hash: $shorter_hash | folder: $context.path_v')
	if context.cmd_to_run.len > 0 {
		scripting.cprintln_strong('#           command: ${context.cmd_to_run:-34s}')
		cmdres := os.execute_or_exit(context.cmd_to_run)
		if cmdres.exit_code != 0 {
			scripting.cprintln_strong('#         exit code: ${cmdres.exit_code:-4d}')
		}
		scripting.cprint_strong('#            result: ')
		print(cmdres.output)
		if !cmdres.output.ends_with('\n') {
			println('')
		}
		exit(cmdres.exit_code)
	}
}
