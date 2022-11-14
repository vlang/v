module os

pub fn mkdir(path string, params MkdirParams) ! {
	$if js_node {
		if path == '.' {
			return
		}
		#$fs.mkdirSync(path.valueOf())

		return
	} $else {
		return error('could not create folder')
	}
}

pub fn is_dir(path string) bool {
	res := false
	$if js_node {
		#res.val = $fs.existsSync(path,str) && $fs.lstatSync(path.str).isDirectory()
	}
	return res
}

pub fn is_link(path string) bool {
	res := false
	$if js_node {
		#res.val = $fs.existsSync(path.str) && $fs.lstatSync(path.str).isSymbolicLink()
	}
	return res
}

struct PathKind {
	is_dir  bool
	is_link bool
}

fn kind_of_existing_path(path string) PathKind {
	is_link := false
	is_dir := false
	$if js_node {
		#is_link.val = $fs.existsSync(path.str) && $fs.lstatSync(path.str).isSymbolicLink()
		#is_dir.val = $fs.existsSync(path,str) && $fs.lstatSync(path.str).isDirectory()
	}
	return PathKind{
		is_dir: is_dir
		is_link: is_link
	}
}

pub fn exists(path string) bool {
	res := false
	$if js_node {
		#res.val = $fs.existsSync(path.str)
	}
	return res
}

pub fn ls(path string) ![]string {
	if !is_dir(path) {
		return error('ls(): cannot open dir ${dir}')
	}

	result := []string{}
	$if js_node {
		#let i = 0
		#$fs.readdirSync(path.str).forEach((path) => result.arr[i++] = new string(path))
	}
	return result
}

pub fn get_raw_line() string {
	return ''
}

pub fn executable() string {
	return ''
}

pub fn is_executable(path string) bool {
	eprintln('TODO: There is no isExecutable on fs.stats')
	return false
}

pub fn rmdir(path string) ! {
	$if js_node {
		err := ''
		#try {
		#$fs.rmdirSync(path.str)
		#return;
		#} catch (e) {
		#err.str = 'Failed to remove "' + path.str + '": ' + e.toString()
		#}

		return error(err)
	}
}

pub fn rm(path string) ! {
	$if js_node {
		err := ''
		#try {
		#$fs.rmSync(path.str)
		#return;
		#} catch (e) {
		#err.str = 'Failed to remove "' + path.str + '": ' + e.toString()
		#}

		return error(err)
	}
}

pub fn cp(src string, dst string) ! {
	$if js_node {
		err := ''
		#try {
		#$fs.cpSync(src.str,dst.str);
		#return;
		#} catch (e) {
		#err.str = 'failed to copy ' + src.str + ' to ' + dst.str + ': ' + e.toString();
		#}

		return error(err)
	}
}

pub fn read_file(s string) !string {
	mut err := ''
	err = err
	res := ''
	#try {
	#res.str = $fs.readFileSync(s.str).toString()
	#} catch (e) {
	#err.str = 'Failed to read file: ' + e.toString()
	#return error(err)
	#}

	return res
}

pub fn getwd() string {
	res := ''
	#res.str = $process.cwd()

	return res
}

pub fn getuid() int {
	res := 0
	#if (process.getuid) res.val = process.getuid();

	return res
}

pub fn execvp(cmd string, args []string) ! {
	panic('os.execvp() is not available on JS backend')
}

pub fn stdin_resume() {
	#$process.stdin.resume();
}

pub fn is_readable(path string) bool {
	$if js_node {
		res := false
		#try { res.val = $fs.accessSync(path.str,$fs.constants.R_OK); } catch { res.val = false; }

		return res
	} $else {
		return false
	}
}
