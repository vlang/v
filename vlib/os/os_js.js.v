module os

pub fn mkdir(path string) ?bool {
	$if js_node {
		if path == '.' {
			return true
		}
		#$fs.mkdirSync(path.valueOf())

		return true
	} $else {
		return false
	}
}

pub fn is_dir(path string) bool {
	res := false
	#res.val = $fs.existsSync(path,str) && $fs.lstatSync(path.str).isDirectory()

	return res
}

pub fn is_link(path string) bool {
	res := false
	#res.val = $fs.existsSync(path.str) && $fs.lstatSync(path.str).isSymbolicLink()

	return res
}

pub fn exists(path string) bool {
	res := false
	#res.val = $fs.existsSync(path.str)

	return res
}

pub fn ls(path string) ?[]string {
	if !is_dir(path) {
		return error('ls(): cannot open dir $dir')
	}

	result := []string{}
	#let i = 0
	#$fs.readdirSync(path.str).forEach((path) => result.arr[i++] = new builtin.string(path))

	return result
}
