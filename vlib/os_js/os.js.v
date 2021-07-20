module os_js

#const $fs = require('fs');
#const $path = require('path');

pub const (
	args = []string{}
)

$if js_node {
	#$process.argv.forEach(function(val,index) { args.arr[index] = new string(val); })
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
