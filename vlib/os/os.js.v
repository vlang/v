module os

#const $fs = require('fs');
#const $path = require('path');

pub const (
	path_delimiter = '/'
	path_separator = '/'
	args           = []string{}
)

$if js_node {
	#$process.argv.forEach(function(val,index) { os__args.arr[index] = new string(val); })
}

// real_path returns the full absolute path for fpath, with all relative ../../, symlinks and so on resolved.
// See http://pubs.opengroup.org/onlinepubs/9699919799/functions/realpath.html
// Also https://insanecoding.blogspot.com/2007/11/pathmax-simply-isnt.html
// and https://insanecoding.blogspot.com/2007/11/implementing-realpath-in-c.html
// NB: this particular rabbit hole is *deep* ...
pub fn real_path(fpath string) string {
	$if js_node {
		mut res := ''
		#res = new string( $fs.realpathSync(fpath))

		return res
	} $else {
		return fpath
	}
}

// flush will flush the stdout buffer.
pub fn flush() {
	$if js_node {
		#$process.stdout.write('')
	}
}

// chmod change file access attributes of `path` to `mode`.
// Octals like `0o600` can be used.
pub fn chmod(path string, mode int) {
	$if js_node {
		#$fs.chmodSync(''+path,mode.valueOf())
	}
}

// chown changes the owner and group attributes of `path` to `owner` and `group`.
// Octals like `0o600` can be used.
pub fn chown(path string, owner int, group int) {
	$if js_node {
		#$fs.chownSync(''+path,owner.valueOf(),group.valueOf())
	}
}

pub fn temp_dir() string {
	mut res := ''
	$if js_node {
		#res = new string($os.tmpdir())
	}
	return res
}

pub fn home_dir() string {
	mut res := ''
	$if js_node {
		#res = new string($os.homedir())
	}
	return res
}

// join_path returns a path as string from input string parameter(s).
pub fn join_path(base string, dirs ...string) string {
	mut result := []string{}
	result << base.trim_right('\\/')
	for d in dirs {
		result << d
	}
	mut path_sep := ''
	#path_sep = $path.sep;

	res := result.join(path_sep)
	return res
}

pub fn execute(cmd string) Result {
	mut exit_code := 0
	mut stdout := ''
	#let commands = cmd.str.split(' ');
	#let output = $child_process.spawnSync(commands[0],commands.slice(1,commands.length));
	#exit_code = new int(output.status)
	#stdout = newstring(output.stdout + '')

	return Result{
		exit_code: exit_code
		output: stdout
	}
}
