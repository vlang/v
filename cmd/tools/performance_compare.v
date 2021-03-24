import os
import flag
import scripting
import vgit

const (
	tool_version     = '0.0.5'
	tool_description = "  Compares V executable size and performance,
|  between 2 commits from V's local git history.
|  When only one commit is given, it is compared to master.
|  ".strip_margin()
)

struct Context {
	cwd string // current working folder
mut:
	vgo           vgit.VGitOptions
	a             string // the full path to the 'after' folder inside workdir
	b             string // the full path to the 'before' folder inside workdir
	vc            string // the full path to the vc folder inside workdir. It is used during bootstrapping v from the C source.
	commit_before string // the git commit for the 'before' state
	commit_after  string // the git commit for the 'after' state
	warmups       int    // how many times to execute a command before gathering stats
	hyperfineopts string // use for additional CLI options that will be given to the hyperfine command
	vflags        string // other v options to pass to compared v commands
}

fn new_context() Context {
	return Context{
		cwd: os.getwd()
		commit_after: 'master'
		warmups: 4
	}
}

fn (c Context) compare_versions() {
	// Input is validated at this point...
	// Cleanup artifacts from previous runs of this tool:
	scripting.chdir(c.vgo.workdir)
	scripting.run('rm -rf "$c.a" "$c.b" "$c.vc" ')
	// clone the VC source *just once per comparison*, and reuse it:
	scripting.run('git clone --quiet "$c.vgo.vc_repo_url" "$c.vc" ')
	println('Comparing V performance of commit $c.commit_before (before) vs commit $c.commit_after (after) ...')
	c.prepare_v(c.b, c.commit_before)
	c.prepare_v(c.a, c.commit_after)
	scripting.chdir(c.vgo.workdir)
	if c.vflags.len > 0 {
		os.setenv('VFLAGS', c.vflags, true)
	}
	// The first is the baseline, against which all the others will be compared.
	// It is the fastest, since hello_world.v has only a single println in it,
	mut perf_files := []string{}
	perf_files << c.compare_v_performance('source_hello', [
		'vprod @DEBUG@ -o source.c examples/hello_world.v',
		'vprod         -o source.c examples/hello_world.v',
		'v     @DEBUG@ -o source.c examples/hello_world.v',
		'v             -o source.c examples/hello_world.v',
	])
	perf_files << c.compare_v_performance('source_v', ['vprod @DEBUG@ -o source.c @COMPILER@',
		'vprod         -o source.c @COMPILER@', 'v     @DEBUG@ -o source.c @COMPILER@',
		'v             -o source.c @COMPILER@',
	])
	perf_files << c.compare_v_performance('binary_hello', [
		'vprod         -o hello    examples/hello_world.v',
		'v             -o hello    examples/hello_world.v',
	])
	perf_files << c.compare_v_performance('binary_v', ['vprod         -o binary   @COMPILER@',
		'v             -o binary   @COMPILER@',
	])
	println('All performance files:')
	for f in perf_files {
		println('   $f')
	}
}

fn (c &Context) prepare_v(cdir string, commit string) {
	mut cc := os.getenv('CC')
	if cc == '' {
		cc = 'cc'
	}
	mut vgit_context := vgit.VGitContext{
		cc: cc
		commit_v: commit
		path_v: cdir
		path_vc: c.vc
		workdir: c.vgo.workdir
		v_repo_url: c.vgo.v_repo_url
		vc_repo_url: c.vgo.vc_repo_url
	}
	vgit_context.compile_oldv_if_needed()
	scripting.chdir(cdir)
	println('Making a v compiler in $cdir')
	scripting.run('./v -cc $cc       -o v     $vgit_context.vvlocation')
	println('Making a vprod compiler in $cdir')
	scripting.run('./v -cc $cc -prod -o vprod $vgit_context.vvlocation')
	println('Stripping and compressing cv v and vprod binaries in $cdir')
	scripting.run('cp    cv     cv_stripped')
	scripting.run('cp     v      v_stripped')
	scripting.run('cp vprod  vprod_stripped')
	scripting.run('strip *_stripped')
	scripting.run('cp cv_stripped cv_stripped_upxed')
	scripting.run('cp  v_stripped  v_stripped_upxed')
	scripting.run('cp vprod_stripped vprod_stripped_upxed')
	scripting.run('upx -qqq --lzma    cv_stripped_upxed')
	scripting.run('upx -qqq --lzma     v_stripped_upxed')
	scripting.run('upx -qqq --lzma vprod_stripped_upxed')
	scripting.show_sizes_of_files(['$cdir/cv', '$cdir/cv_stripped', '$cdir/cv_stripped_upxed'])
	scripting.show_sizes_of_files(['$cdir/v', '$cdir/v_stripped', '$cdir/v_stripped_upxed'])
	scripting.show_sizes_of_files(['$cdir/vprod', '$cdir/vprod_stripped',
		'$cdir/vprod_stripped_upxed',
	])
	vversion := scripting.run('$cdir/v -version')
	vcommit := scripting.run('git rev-parse --short  --verify HEAD')
	println('V version is: $vversion , local source commit: $vcommit')
	if vgit_context.vvlocation == 'cmd/v' {
		if os.exists('vlib/v/ast/ast.v') {
			println('Source lines of the compiler: ' +
				scripting.run('find cmd/v/ vlib/v/ -name "*.v" | grep -v /tests/ | xargs wc | tail -n -1'))
		} else {
			println('Source lines of the compiler: ' +
				scripting.run('wc cmd/v/*.v vlib/compiler/*.v | tail -n -1'))
		}
	} else if vgit_context.vvlocation == 'v.v' {
		println('Source lines of the compiler: ' +
			scripting.run('wc v.v vlib/compiler/*.v | tail -n -1'))
	} else {
		println('Source lines of the compiler: ' + scripting.run('wc compiler/*.v | tail -n -1'))
	}
}

fn (c Context) compare_v_performance(label string, commands []string) string {
	println('---------------------------------------------------------------------------------')
	println('Compare v performance when doing the following commands ($label):')
	mut source_location_a := ''
	mut source_location_b := ''
	if os.exists('$c.a/cmd/v') {
		source_location_a = 'cmd/v'
	} else {
		source_location_a = if os.exists('$c.a/v.v') { 'v.v       ' } else { 'compiler/ ' }
	}
	if os.exists('$c.b/cmd/v') {
		source_location_b = 'cmd/v'
	} else {
		source_location_b = if os.exists('$c.b/v.v') { 'v.v       ' } else { 'compiler/ ' }
	}
	timestamp_a, _ := vgit.line_to_timestamp_and_commit(scripting.run('cd $c.a/ ; git rev-list -n1 --timestamp HEAD'))
	timestamp_b, _ := vgit.line_to_timestamp_and_commit(scripting.run('cd $c.b/ ; git rev-list -n1 --timestamp HEAD'))
	debug_option_a := if timestamp_a > 1570877641 { '-cg    ' } else { '-debug ' }
	debug_option_b := if timestamp_b > 1570877641 { '-cg    ' } else { '-debug ' }
	mut hyperfine_commands_arguments := []string{}
	for cmd in commands {
		println(cmd)
	}
	for cmd in commands {
		hyperfine_commands_arguments << " \'cd ${c.b:-34s} ; ./$cmd \' ".replace_each([
			'@COMPILER@',
			source_location_b,
			'@DEBUG@',
			debug_option_b,
		])
	}
	for cmd in commands {
		hyperfine_commands_arguments << " \'cd ${c.a:-34s} ; ./$cmd \' ".replace_each([
			'@COMPILER@',
			source_location_a,
			'@DEBUG@',
			debug_option_a,
		])
	}
	// /////////////////////////////////////////////////////////////////////////////
	cmd_stats_file := os.real_path([c.vgo.workdir, 'v_performance_stats_${label}.json'].join(os.path_separator))
	comparison_cmd := 'hyperfine $c.hyperfineopts ' + '--export-json $cmd_stats_file ' +
		'--time-unit millisecond ' + '--style full --warmup $c.warmups ' +
		hyperfine_commands_arguments.join(' ')
	// /////////////////////////////////////////////////////////////////////////////
	if c.vgo.verbose {
		println(comparison_cmd)
	}
	os.system(comparison_cmd)
	println('The detailed performance comparison report was saved to: $cmd_stats_file .')
	println('')
	return cmd_stats_file
}

fn main() {
	scripting.used_tools_must_exist(['cp', 'rm', 'strip', 'make', 'git', 'upx', 'cc', 'wc', 'tail',
		'find', 'xargs', 'hyperfine'])
	mut context := new_context()
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.file_name(os.executable()))
	fp.version(tool_version)
	fp.description(tool_description)
	fp.arguments_description('COMMIT_BEFORE [COMMIT_AFTER]')
	fp.skip_executable()
	fp.limit_free_args(1, 2)
	context.vflags = fp.string('vflags', 0, '', 'Additional options to pass to the v commands, for example "-cc tcc"')
	context.hyperfineopts = fp.string('hyperfine_options', 0, '', 'Additional options passed to hyperfine.
${flag.space}For example on linux, you may want to pass:
$flag.space--hyperfine_options "--prepare \'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches\'"
')
	commits := vgit.add_common_tool_options(mut context.vgo, mut fp)
	context.commit_before = commits[0]
	if commits.len > 1 {
		context.commit_after = commits[1]
	}
	context.b = vgit.normalized_workpath_for_commit(context.vgo.workdir, context.commit_before)
	context.a = vgit.normalized_workpath_for_commit(context.vgo.workdir, context.commit_after)
	context.vc = vgit.normalized_workpath_for_commit(context.vgo.workdir, 'vc')
	if !os.is_dir(context.vgo.workdir) {
		msg := 'Work folder: ' + context.vgo.workdir + ' , does not exist.'
		eprintln(msg)
		exit(2)
	}
	context.compare_versions()
}
