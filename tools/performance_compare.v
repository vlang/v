import (
	os
	flag
	filepath
)

const (
	tool_version = '0.0.4'
	tool_description = '' +
		'  Compares V executable size and performance,\n' +
		'  between 2 commits from V\'s local git history.\n' +
		'  When only one commit is given, it is compared to master.'
)

struct Context {
	cwd			  string // current working folder
mut:
	vc_repo_url	  string // the url of the vc repository. It can be a local folder path, which is usefull to eliminate network operations...
	workdir		  string // the working folder (typically /tmp), where the tool will write
	a			  string // the full path to the 'after' folder inside workdir
	b			  string // the full path to the 'before' folder inside workdir
	vc			  string // the full path to the vc folder inside workdir. It is used during bootstrapping v from the C source.
	commit_before string // the git commit for the 'before' state
	commit_after  string // the git commit for the 'after' state
	warmups		  int    // how many times to execute a command before gathering stats
	verbose		  bool   // whether to print even more stuff
	hyperfineopts string // use for additional CLI options that will be given to the hyperfine command
}
fn new_context() Context {
	return Context{ cwd: os.getwd(), commit_after: 'master', warmups: 4 }
}

////// The stuff in this block may be reusable for other v cli tools? /////////////////
fn run(cmd string) string {
	x := os.exec(cmd) or { return '' }
	if x.exit_code == 0 { return x.output }
	return ''
}

fn command_exits_with_zero_status(cmd string) bool {
	x := os.exec(cmd) or { return false }
	if x.exit_code == 0 { return true }
	return false
}

fn tool_must_exist(toolcmd string) {
	if command_exits_with_zero_status( 'type $toolcmd' ) { return }
	eprintln('Missing tool: $toolcmd')
	eprintln('Please try again after you install it.')
	exit(1)
}

fn used_tools_must_exist(tools []string) {
	for t in tools {
		tool_must_exist(t)
	}
}
//////////////////////////////////////////////////////////////////////////

fn (c Context) compare_versions() {
	// Input is validated at this point...
	// Cleanup artifacts from previous runs of this tool:
	os.chdir( c.workdir )
	run('rm -rf "$c.a" "$c.b" "$c.vc" ')
	// clone the VC source *just once per comparison*, and reuse it:
	run('git clone --quiet \'$c.vc_repo_url\'	\'$c.vc\' ')

	println('Comparing V performance of commit $c.commit_before (before) vs commit $c.commit_after (after) ...')
	c.prepare_v( c.b , c.commit_before )
	c.prepare_v( c.a , c.commit_after  )

	os.chdir( c.workdir )
	//The first is the baseline, against which all the others will be compared.
	//It is the fastest, since hello_world.v has only a single println in it,
	c.compare_v_performance([
			'vprod @DEBUG@ -o source.c examples/hello_world.v',
			'vprod		   -o source.c examples/hello_world.v',
			'vprod @DEBUG@ -o source.c @COMPILER@',
			'vprod		   -o source.c @COMPILER@',
			'vprod		   -o hello	   examples/hello_world.v',
			'vprod		   -o binary   @COMPILER@',
			/////////////////////////////////////////////////////////
			'v	  @DEBUG@  -o source.c examples/hello_world.v',
			'v			   -o source.c examples/hello_world.v',
			'v	  @DEBUG@  -o source.c @COMPILER@',
			'v			   -o source.c @COMPILER@',
			'v			   -o hello	   examples/hello_world.v',
			'v			   -o binary   @COMPILER@',
			])

}

fn show_sizes_of_files(files []string) {
	for f in files {
		size := os.file_size(f)
		println('${size:10d} $f')
	}
}

fn line_to_timestamp_and_commit(line string) (int, string) {
	parts := line.split(' ')
	return parts[0].int(), parts[1]
}

fn (c &Context) prepare_vc_source( cdir string, commit string ) {
	os.chdir( cdir )
	// Building a historic v with the latest vc is not always possible ...
	// It is more likely, that the vc *at the time of the v commit*,
	// or slightly before that time will be able to build the historic v:
	vline := run('git rev-list -n1 --timestamp \'$commit\' ')
	v_timestamp, _ := line_to_timestamp_and_commit( vline )
	os.chdir( c.vc )
	run('git checkout master')
	vcbefore := run('git rev-list HEAD -n1 --timestamp --before=$v_timestamp ')
	_, vccommit_before := line_to_timestamp_and_commit( vcbefore )
	run('git checkout \'$vccommit_before\' ')
	os.chdir( cdir )
}

fn (c &Context) prepare_v( cdir string, commit string ) {
	mut cc := os.getenv('CC')
	if cc == '' { cc = 'cc' }

	mut command_for_building_v_from_c_source := '$cc -std=gnu11 -w -o cv	 $c.vc/v.c	   -lm'
	if 'windows' == os.user_os() {
		command_for_building_v_from_c_source  = '$cc -std=gnu11 -w -o cv.exe $c.vc/v_win.c'
	}

	println('')
	// prepare c.vc first
	os.chdir( c.vc )
	run('git checkout master')

	println('Cloning current v source to $cdir ...')
	os.system('git clone --quiet \'$c.cwd\' \'$cdir\' ')
	os.chdir( cdir )
	os.system('git checkout --quiet \'$commit\' ')

	run('git clean -f')
	c.prepare_vc_source( cdir, commit )
	source_location := if os.exists('v.v') { 'v.v' } else { 'compiler/' }

	println('Making v and vprod compilers in $cdir')
	run(command_for_building_v_from_c_source)
	run('./cv		-o v	 $source_location')
	run('./cv -prod -o vprod $source_location')

	run('cp cv		 cv_stripped')
	run('cp v		 v_stripped')
	run('cp vprod	 vprod_stripped')
	run('strip *_stripped')

	run('cp cv_stripped		 cv_stripped_upxed')
	run('cp v_stripped		 v_stripped_upxed')
	run('cp vprod_stripped	 vprod_stripped_upxed')
	run('upx -qqq --lzma cv_stripped_upxed')
	run('upx -qqq --lzma v_stripped_upxed')
	run('upx -qqq --lzma vprod_stripped_upxed')

	show_sizes_of_files(["$cdir/cv",	"$cdir/cv_stripped",	 "$cdir/cv_stripped_upxed"])
	show_sizes_of_files(["$cdir/v",		"$cdir/v_stripped",		 "$cdir/v_stripped_upxed"])
	show_sizes_of_files(["$cdir/vprod", "$cdir/vprod_stripped",	 "$cdir/vprod_stripped_upxed"])
	println("V version is: " + run("$cdir/v --version") + " , local source commit: " + run("git rev-parse --short  --verify HEAD") )
	println('Source lines of the compiler: ' + run('wc v.v compiler/*.v vlib/compiler/*.v | tail -n -1') )
}

fn (c Context) compare_v_performance( commands []string ) {
	println('---------------------------------------------------------------------------------')
	println('Compare v performance when doing the following commands:')

	source_location_a := if os.exists('$c.a/v.v') { 'v.v' } else { 'compiler/' }
	source_location_b := if os.exists('$c.b/v.v') { 'v.v' } else { 'compiler/' }
	timestamp_a, _ := line_to_timestamp_and_commit(run('cd $c.a/ ; git rev-list -n1 --timestamp HEAD'))
	timestamp_b, _ := line_to_timestamp_and_commit(run('cd $c.b/ ; git rev-list -n1 --timestamp HEAD'))
	debug_option_a := if timestamp_a > 1570877641 { '-g' } else { '-debug' }
	debug_option_b := if timestamp_b > 1570877641 { '-g' } else { '-debug' }

	mut hyperfine_commands_arguments := []string
	for cmd in commands { println(cmd) }
	for cmd in commands {
		hyperfine_commands_arguments << ' \'cd ${c.b:30s} ; ./$cmd \' '.replace_each(['@COMPILER@', source_location_b, '@DEBUG@', debug_option_b])
	}
	for cmd in commands {
		hyperfine_commands_arguments << ' \'cd ${c.a:30s} ; ./$cmd \' '.replace_each(['@COMPILER@', source_location_a, '@DEBUG@', debug_option_a])
	}
	///////////////////////////////////////////////////////////////////////////////
	cmd_stats_file := os.realpath([ c.workdir, 'v_performance_stats.json'].join(os.path_separator))
	comparison_cmd := 'hyperfine $c.hyperfineopts '+
		'--export-json ${cmd_stats_file} '+
		'--time-unit millisecond '+
		'--style full --warmup $c.warmups ' +
		hyperfine_commands_arguments.join(' ')
	///////////////////////////////////////////////////////////////////////////////
	if c.verbose { println( comparison_cmd ) }
	os.system( comparison_cmd )
	println('The detailed performance comparison report was saved to: $cmd_stats_file .')
	println('')
}

fn (c Context) normalized_workpath_for_commit( commit string ) string {
	nc := 'v_at_' + commit.replace('^','_').replace('-','_').replace('/','_')
	return os.realpath( c.workdir + os.path_separator + nc )
}

fn validate_commit_exists( commit string ){
	cmd := 'git cat-file -t ' + "'" + commit + "'"
	if !command_exits_with_zero_status(cmd) {
		eprintln("Commit: '" + commit + "' does not exist in the current repository.")
		exit(3)
	}
}

fn main(){
	used_tools_must_exist(['cp','rm','strip','make','git','upx','cc','wc','tail','hyperfine'])
	mut context := new_context()
	mut fp := flag.new_flag_parser(os.args)
	fp.application(filepath.filename(os.executable()))
	fp.version( tool_version )
	fp.description( tool_description )
	fp.arguments_description('COMMIT_BEFORE [COMMIT_AFTER]')
	fp.skip_executable()
	fp.limit_free_args(1,2)
	show_help:=fp.bool('help', false, 'Show this help screen\n')

	context.vc_repo_url = fp.string('vcrepo', 'https://github.com/vlang/vc',
		'' +
			'The url of the vc repository. You can clone it \n'+
			flag.SPACE+'beforehand, and then just give the local folder \n'+
			flag.SPACE+'path here. That will eliminate the network ops	\n'+
			flag.SPACE+'done by this tool, which is useful, if you want \n'+
			flag.SPACE+'to script it/run it in a restrictive vps/docker.\n')

	context.verbose = fp.bool('verbose', false, 'Be more verbose\n')
	context.hyperfineopts = fp.string('hyperfine_options', '',
		'' +
			'Additional options passed to hyperfine.\n'+
			flag.SPACE+'For example on linux, you may want to pass:\n'+
			flag.SPACE+'   --hyperfine_options "--prepare \'sync; echo 3 | sudo tee /proc/sys/vm/drop_caches\'" \n')

	context.workdir = os.realpath( fp.string('workdir', '/tmp', 'A writable folder, where the comparison will be done.') )
	if( show_help ){
		println( fp.usage() )
		exit(0)
	}
	commits := fp.finalize() or {
		eprintln('Error: ' + err)
		exit(1)
	}

	context.commit_before = commits[0]
	if commits.len > 1 { context.commit_after = commits[1] }

	validate_commit_exists( context.commit_before )
	validate_commit_exists( context.commit_after )

	context.b  = context.normalized_workpath_for_commit( context.commit_before )
	context.a  = context.normalized_workpath_for_commit( context.commit_after )
	context.vc = context.normalized_workpath_for_commit( 'vc' )

	if !os.is_dir( context.workdir ) {
		msg := 'Work folder: ' + context.workdir + ' , does not exist.'
		eprintln(msg)
		exit(2)
	}

	context.compare_versions()
}
