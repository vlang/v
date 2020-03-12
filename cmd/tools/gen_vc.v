// This tool regenerates V's bootstrap .c files
// every time the V master branch is updated.

// if run with the --serve flag it will run in webhook
// server mode awaiting a request to http://host:port/genhook

// available command line flags:
// --work-dir  gen_vc's working directory
// --purge     force purge the local repositories
// --serve     run in webhook server mode
// --port      port for http server to listen on
// --log-to    either 'file' or 'terminal'
// --log-file  path to log file used when --log-to is 'file'
// --dry-run   dont push anything to remote repo

module main

import (
	os
	log
	flag
	time
	vweb
	net.urllib
)

// git credentials
const(
	git_username = os.getenv('GITUSER')
	git_password = os.getenv('GITPASS')
)

// repository
const(
	// git repo
	git_repo_v  = 'github.com/vlang/v'
	git_repo_vc = 'github.com/vlang/vc'
	// local repo directories
	git_repo_dir_v  = 'v'
	git_repo_dir_vc = 'vc'
)

// gen_vc
const(
	// name
	app_name = 'gen_vc'
	// version
	app_version = '0.1.1'
	// description
	app_description = 'This tool regenerates V\'s bootstrap .c files every time the V master branch is updated.'
	// assume something went wrong if file size less than this
	too_short_file_limit = 5000
	// create a .c file for these os's
	vc_build_oses = [
		'nix', // all nix based os
		'windows'
	]
)

// default options (overridden by flags)
const(
	// gen_vc working directory
	work_dir = '/tmp/gen_vc'
	// dont push anything to remote repo
	dry_run = false
	// server port
	server_port = 7171
	// log file
	log_file = '$work_dir/log.txt'
	// log_to is either 'file' or 'terminal'
	log_to = 'terminal'
)

// errors
const(
	err_msg_build = 'error building'
	err_msg_make  = 'make failed'
	err_msg_gen_c = 'failed to generate .c file'
	err_msg_cmd_x = 'error running cmd'
)

struct GenVC {
	// logger
	logger &log.Log
	// flag options
	options FlagOptions
mut:
	// true if error was experienced running generate
	gen_error bool
}

// webhook server
pub struct WebhookServer {
pub mut:
	vweb   vweb.Context
	gen_vc &GenVC
}

// storage for flag options
struct FlagOptions {
	work_dir string
	purge    bool
	serve    bool
	port     int
	log_to   string
	log_file string
	dry_run  bool
}

fn main() {
	mut fp := flag.new_flag_parser(os.args.clone())

	fp.application(app_name)
 	fp.version(app_version)
 	fp.description(app_description)
 	fp.skip_executable()

	show_help:=fp.bool('help', false, 'Show this help screen\n')
	flag_options := parse_flags(mut fp)

	if( show_help ){ println( fp.usage() ) exit(0) }

	fp.finalize() or {
 		eprintln(err)
 		println(fp.usage())
 		return
 	}

	// webhook server mode
	if flag_options.serve {
		vweb.run<WebhookServer>(flag_options.port)
	}
	// cmd mode
	else {
		mut gen_vc := new_gen_vc(flag_options)
		gen_vc.init()
		gen_vc.generate()
	}
}

// new GenVC
fn new_gen_vc(flag_options FlagOptions) &GenVC {
	mut logger := &log.Log{}
	logger.set_level(log.DEBUG)
	if flag_options.log_to == 'file' {
		logger.set_full_logpath( flag_options.log_file )
	}
	return &GenVC{
		options: flag_options
		logger: logger
	}
}

// WebhookServer init
pub fn (ws mut WebhookServer) init() {

	mut fp := flag.new_flag_parser(os.args.clone())
	flag_options := parse_flags(mut fp)
	ws.gen_vc = new_gen_vc(flag_options)
	ws.gen_vc.init()
	//ws.gen_vc = new_gen_vc(flag_options)
}

// gen webhook
pub fn (ws mut WebhookServer) genhook() {
	ws.gen_vc.generate()
	// error in generate
	if ws.gen_vc.gen_error {
		ws.vweb.json('{status: "failed"}')
		return
	}
	ws.vweb.json('{status: "ok"}')
}

pub fn (ws &WebhookServer) reset() {
}


// parse flags to FlagOptions struct
fn parse_flags(fp mut flag.FlagParser) FlagOptions {
	return FlagOptions{
		serve    : fp.bool('serve', false, 'run in webhook server mode')
		work_dir : fp.string('work-dir', work_dir, 'gen_vc working directory')
		purge    : fp.bool('purge', false, 'force purge the local repositories')
		port     : fp.int('port', server_port, 'port for web server to listen on')
		log_to   : fp.string('log-to', log_to, 'log to is \'file\' or \'terminal\'')
		log_file : fp.string('log-file', log_file, 'log file to use when log-to is \'file\'')
		dry_run  : fp.bool('dry-run', dry_run, 'when specified dont push anything to remote repo')
	}
}

// init
fn (gen_vc mut GenVC) init() {
	// purge repos if flag is passed
	if gen_vc.options.purge {
		gen_vc.purge_repos()
	}
}

// regenerate
fn (gen_vc mut GenVC) generate() {
	// set errors to false
	gen_vc.gen_error = false

	// check if gen_vc dir exists
	if !os.is_dir(gen_vc.options.work_dir) {
		// try create
		os.mkdir(gen_vc.options.work_dir) or { panic(err) }
		// still dosen't exist... we have a problem
		if !os.is_dir(gen_vc.options.work_dir) {
			gen_vc.logger.error('error creating directory: $gen_vc.options.work_dir')
			gen_vc.gen_error = true
			return
		}
	}

	// cd to gen_vc dir
	os.chdir(gen_vc.options.work_dir)

	// if we are not running with the --serve flag (webhook server)
	// rather than deleting and re-downloading the repo each time
	// first check to see if the local v repo is behind master
	// if it isn't behind theres no point continuing further
	if !gen_vc.options.serve && os.is_dir(git_repo_dir_v) {
		gen_vc.cmd_exec('git -C $git_repo_dir_v checkout master')
		// fetch the remote repo just in case there are newer commits there
		gen_vc.cmd_exec('git -C $git_repo_dir_v fetch')
		git_status := gen_vc.cmd_exec('git -C $git_repo_dir_v status')
		if !git_status.contains('behind') {
			gen_vc.logger.warn('v repository is already up to date.')
			return
		}
	}

	// delete repos
	gen_vc.purge_repos()

	// clone repos
	gen_vc.cmd_exec('git clone --depth 1 https://$git_repo_v $git_repo_dir_v')
	gen_vc.cmd_exec('git clone --depth 1 https://$git_repo_vc $git_repo_dir_vc')

	// get output of git log -1 (last commit)
	git_log_v := gen_vc.cmd_exec('git -C $git_repo_dir_v log -1 --format="commit %H%nDate: %ci%nDate Unix: %ct%nSubject: %s"')
	git_log_vc := gen_vc.cmd_exec('git -C $git_repo_dir_vc log -1 --format="Commit %H%nDate: %ci%nDate Unix: %ct%nSubject: %s"')

	// date of last commit in each repo
	ts_v := git_log_v.find_between('Date:', '\n').trim_space()
	ts_vc := git_log_vc.find_between('Date:', '\n').trim_space()

	// parse time as string to time.Time
	last_commit_time_v  := time.parse(ts_v) or {
		panic(err)
	}
	last_commit_time_vc := time.parse(ts_vc) or {
		panic(err)
	}

	// git dates are in users local timezone and v time.parse does not parse
	// timezones at the moment, so for now get unix timestamp from output also
	t_unix_v := git_log_v.find_between('Date Unix:', '\n').trim_space().int()
	t_unix_vc := git_log_vc.find_between('Date Unix:', '\n').trim_space().int()

	// last commit hash in v repo
	last_commit_hash_v := git_log_v.find_between('commit', '\n').trim_space()
	last_commit_hash_v_short := last_commit_hash_v[..7]

	// subject
	last_commit_subject := git_log_v.find_between('Subject:', '\n').trim_space()

	// log some info
	gen_vc.logger.debug('last commit time ($git_repo_v): ' + last_commit_time_v.format_ss())
	gen_vc.logger.debug('last commit time ($git_repo_vc): ' + last_commit_time_vc.format_ss())
	gen_vc.logger.debug('last commit hash ($git_repo_v): $last_commit_hash_v')
	gen_vc.logger.debug('last commit subject ($git_repo_v): $last_commit_subject')

	// if vc repo already has a newer commit than the v repo, assume it's up to date
	if t_unix_vc >= t_unix_v {
		gen_vc.logger.warn('vc repository is already up to date.')
		return
	}

	// try build v for current os (linux in this case)
	gen_vc.cmd_exec('make -C $git_repo_dir_v')
	v_exec := '$git_repo_dir_v/v'
	// check if make was successful
	gen_vc.assert_file_exists_and_is_not_too_short(v_exec, err_msg_make)

	// build v.c for each os
	for os_name in vc_build_oses {
		vc_suffix := if os_name == 'nix' { '' } else { '_${os_name[..3]}' }
		v_flags := if os_name == 'nix' { '-output-cross-platform-c' } else { '-os $os_name' }
		c_file := 'v${vc_suffix}.c'
		// try generate .c file
		gen_vc.cmd_exec('$v_exec $v_flags -o $c_file $git_repo_dir_v/cmd/v')
		// check if the c file seems ok
		gen_vc.assert_file_exists_and_is_not_too_short(c_file, err_msg_gen_c)
		// embed the latest v commit hash into the c file
		gen_vc.cmd_exec('sed -i \'1s/^/#define V_COMMIT_HASH "$last_commit_hash_v_short"\\n/\' $c_file')
		// run clang-format to make the c file more readable
		gen_vc.cmd_exec('clang-format -i $c_file')
		// move to vc repo
		gen_vc.cmd_exec('mv $c_file $git_repo_dir_vc/$c_file')
		// add new .c file to local vc repo
		gen_vc.cmd_exec('git -C $git_repo_dir_vc add $c_file')
	}

	// check if the vc repo actually changed
	git_status := gen_vc.cmd_exec('git -C $git_repo_dir_vc status')
	if git_status.contains('nothing to commit') {
		gen_vc.logger.error('no changes to vc repo: something went wrong.')
		gen_vc.gen_error = true
	}
	// commit changes to local vc repo
	gen_vc.cmd_exec_safe('git -C $git_repo_dir_vc commit -m "[v:master] $last_commit_hash_v_short - $last_commit_subject"')
	// push changes to remote vc repo
	gen_vc.cmd_exec_safe('git -C $git_repo_dir_vc push https://${urllib.query_escape(git_username)}:${urllib.query_escape(git_password)}@$git_repo_vc master')
}

// only execute when dry_run option is false, otherwise just log
fn (gen_vc mut GenVC) cmd_exec_safe(cmd string) string {
	return gen_vc.command_execute(cmd, gen_vc.options.dry_run)
}

// always execute command
fn (gen_vc mut GenVC) cmd_exec(cmd string) string {
	return gen_vc.command_execute(cmd, false)
}

// execute command
fn (gen_vc mut GenVC) command_execute(cmd string, dry bool) string {
	// if dry is true then dont execute, just log
	if dry {
		return gen_vc.command_execute_dry(cmd)
	}
	gen_vc.logger.info('cmd: $cmd')
	r := os.exec(cmd) or {
		gen_vc.logger.error('$err_msg_cmd_x: "$cmd" could not start.')
		gen_vc.logger.error( err )
		// something went wrong, better start fresh next time
		gen_vc.purge_repos()
		gen_vc.gen_error = true
		return ''
	}
	if r.exit_code != 0 {
		gen_vc.logger.error('$err_msg_cmd_x: "$cmd" failed.')
		gen_vc.logger.error(r.output)
		// something went wrong, better start fresh next time
		gen_vc.purge_repos()
		gen_vc.gen_error = true
		return ''
	}
	return r.output
}

// just log cmd, dont execute
fn (gen_vc mut GenVC) command_execute_dry(cmd string) string {
	gen_vc.logger.info('cmd (dry): "$cmd"')
	return ''
}

// delete repo directories
fn (gen_vc mut GenVC) purge_repos() {
	// delete old repos (better to be fully explicit here, since these are destructive operations)
	mut repo_dir := '$gen_vc.options.work_dir/$git_repo_dir_v'
	if os.is_dir(repo_dir) {
		gen_vc.logger.info('purging local repo: "$repo_dir"')
		gen_vc.cmd_exec('rm -rf $repo_dir')
	}
	repo_dir = '$gen_vc.options.work_dir/$git_repo_dir_vc'
	if os.is_dir(repo_dir) {
		gen_vc.logger.info('purging local repo: "$repo_dir"')
		gen_vc.cmd_exec('rm -rf $repo_dir')
	}
}

// check if file size is too short
fn (gen_vc mut GenVC) assert_file_exists_and_is_not_too_short(f string, emsg string){
	if !os.exists(f) {
		gen_vc.logger.error('$err_msg_build: $emsg .')
		gen_vc.gen_error = true
		return
	}
	fsize := os.file_size(f)
	if fsize < too_short_file_limit {
		gen_vc.logger.error('$err_msg_build: $f exists, but is too short: only $fsize bytes.')
		gen_vc.gen_error = true
		return
	}
}
