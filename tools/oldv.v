import (
	os
	flag
	filepath
	scripting
)  

const (
	tool_version = '0.0.2'
	tool_description = 'Checkout an old V and compile it. Useful when you want to discover when something broke.'
	remote_repo_url_v  = 'https://github.com/vlang/v'
	remote_repo_url_vc = 'https://github.com/vlang/vc'
)

struct Context {
mut:
	repo_url_v    string            // the url of the V repository. It can be a local folder path, if you want to eliminate network operations...
	repo_url_vc   string            // the url of the vc repository. It can be a local folder path, if you want to eliminate network operations...
	workdir       string            // the working folder (typically /tmp), where the tool will write
	commit_v      string = 'master' // the commit from which you want to produce a working v compiler (this may be a commit-ish too)
	commit_vc     string = 'master' // this will be derived from commit_v
	commit_v_hash string            // this will be filled from the commit-ish commit_v using rev-list. It IS a commit hash.
	path_v        string            // the full path to the v folder inside workdir.
	path_vc       string            // the full path to the vc folder inside workdir.
	cmd_to_run    string            // the command that you want to run *in* the oldv repo
	cc            string = 'cc'     // the C compiler to use for bootstrapping.
	cleanup       bool              // should the tool run a cleanup first
	verbose       bool              // should the tool be much more verbose
}

fn (c mut Context) compile_oldv_if_needed() {  
	vexename := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	vexepath := filepath.join( c.path_v, vexename )
	
	mut command_for_building_v_from_c_source := ''
	mut commands_for_selfbuilding := []string
	if 'windows' == os.user_os() {
		command_for_building_v_from_c_source = '$c.cc -w -o cv.exe  "$c.path_vc/v_win.c" '
		commands_for_selfbuilding << './cv.exe -o v2.exe {SOURCE}'
		commands_for_selfbuilding << './v2.exe -o  $vexename {SOURCE}'
	}else{
		command_for_building_v_from_c_source = '$c.cc -w -o cv      "$c.path_vc/v.c"  -lm'
		commands_for_selfbuilding << './cv -o $vexename {SOURCE}'
	}
	
	scripting.chdir( c.workdir )
	scripting.run('git clone --quiet "$c.repo_url_v"	"$c.path_v" ')
	scripting.run('git clone --quiet "$c.repo_url_vc"	"$c.path_vc" ')
	
	scripting.chdir( c.path_v )
	scripting.run('git checkout $c.commit_v')
	c.prepare_vc_source( c.commit_v )
	
	if os.is_dir( c.path_v ) && os.exists( vexepath ) { return }
	
	scripting.run('git clean -f')  
	source_location := if os.exists('v.v') { 'v.v' } else { 'compiler' }
	scripting.run( command_for_building_v_from_c_source )
	for cmd in commands_for_selfbuilding {
		build_cmd := cmd.replace('{SOURCE}', source_location)
		scripting.run( build_cmd )
	}

	if !os.exists( vexepath ) && c.cmd_to_run.len > 0 {
		// NB: 125 is a special code, that git bisect understands as 'skip this commit'.
		// it is used to inform git bisect that the current commit leads to a build failure.
		exit( 125 ) 
	}

}

fn line_to_timestamp_and_commit(line string) (int, string) {
	parts := line.split(' ')
	return parts[0].int(), parts[1]
}

fn (c mut Context) prepare_vc_source( commit string ) {
	scripting.chdir( c.path_v )
	// Building a historic v with the latest vc is not always possible ...
	// It is more likely, that the vc *at the time of the v commit*,
	// or slightly before that time will be able to build the historic v:
	vline := scripting.run('git rev-list -n1 --timestamp "$commit" ')
	v_timestamp, v_commithash := line_to_timestamp_and_commit( vline )
	c.commit_v_hash = v_commithash
	scripting.check_v_commit_timestamp_before_self_rebuilding(v_timestamp)
	scripting.chdir( c.path_vc )
	scripting.run('git checkout master')
	vcbefore := scripting.run('git rev-list HEAD -n1 --timestamp --before=$v_timestamp ')
	_, vccommit_before := line_to_timestamp_and_commit( vcbefore )
	c.commit_vc = vccommit_before
	scripting.run('git checkout "$vccommit_before" ')
	scripting.chdir( c.path_v )
}

fn (c Context) normalized_workpath_for_commit( commit string ) string {
	nc := 'v_at_' + commit.replace('^','_').replace('-','_').replace('/','_')
	return os.realpath( c.workdir + os.path_separator + nc )
}

fn validate_commit_exists( commit string ){
	cmd := 'git cat-file -t ' + "'" + commit + "'"
	if !scripting.command_exits_with_zero_status(cmd) {
		eprintln("Commit: '" + commit + "' does not exist in the current repository.")
		exit(3)
	}
}

fn main(){
	scripting.used_tools_must_exist(['git','cc'])
	mut context := Context{}
	mut fp := flag.new_flag_parser(os.args)
	fp.application(os.filename(os.executable()))
	fp.version( tool_version )
	fp.description( tool_description )
	fp.arguments_description('VCOMMIT')
	fp.skip_executable()
	
	show_help:=fp.bool('help', false, 'Show this help screen\n')
	context.cmd_to_run = fp.string('command', '', 'Command to run in the old V repo.\n')
	context.cleanup    = fp.bool('clean', true, 'Clean before running (slower).\n')
	context.verbose    = fp.bool('verbose', false, 'Be more verbose.\n')
	
	context.workdir = os.realpath( fp.string('work-dir', os.tmpdir(), 'A writable folder, where the comparison will be done.\n') )
	
	context.repo_url_v = fp.string('v-repo', remote_repo_url_v, 'The url of the V repository. You can clone it locally too.\n')
	
	context.repo_url_vc = fp.string('vc-repo', remote_repo_url_vc, '' +
		'The url of the vc repository. You can clone it \n'+
		flag.SPACE+'beforehand, and then just give the local folder \n'+
		flag.SPACE+'path here. That will eliminate the network ops  \n'+
		flag.SPACE+'done by this tool, which is useful, if you want \n'+
		flag.SPACE+'to script it/run it in a restrictive vps/docker.\n')
	
	if( show_help ){
		println( fp.usage() )
		exit(0)
	}
	
	if context.verbose {
		os.setenv('VERBOSE','true',true)
	}
	
	commits := fp.finalize() or {
		eprintln('Error: ' + err)
		exit(1)
	}
	
	if commits.len > 0 {
		context.commit_v = commits[0]
		validate_commit_exists( context.commit_v )
	}else{
		context.commit_v = scripting.run('git rev-list -n1 HEAD')
	}
	println('#################  context.commit_v: $context.commit_v #####################')
	
	context.path_v  = context.normalized_workpath_for_commit( context.commit_v )
	context.path_vc = context.normalized_workpath_for_commit( 'vc' )
	
	if !os.is_dir( context.workdir ) {
		msg := 'Work folder: ' + context.workdir + ' , does not exist.'
		eprintln(msg)
		exit(2)
	}
	
	ecc := os.getenv('CC')
	if ecc!='' { context.cc = ecc }
	
	if context.cleanup {
		scripting.run('rm -rf $context.path_v')
		scripting.run('rm -rf $context.path_vc')
	}
	
	context.compile_oldv_if_needed()
	
	scripting.chdir( context.path_v )
	println('#     v commit hash: $context.commit_v_hash')
	println('#   checkout folder: $context.path_v')
	
	if context.cmd_to_run.len > 0 {
		cmdres := os.exec( context.cmd_to_run ) or { panic(err) }
		println('#           command: $context.cmd_to_run')
		println('# command exit code: $cmdres.exit_code')
		println('# command result   :')
		println(cmdres.output)
		exit( cmdres.exit_code )
	}
	
}
