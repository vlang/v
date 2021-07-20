module os_js
pub struct File {
	pub:
		fd int 
	pub mut: 
		is_opened bool
}

pub fn open_file(path string,mode string, options ...int) ?File {
	mut res := File {}
	$if js_node {
	# if (!options) { options = new array([]); }
	# let permissions = 0o666
	# if (options.arr.length > 0) { permissions = options.arr[0]; }
	# try {
	# res.fd = new int($fs.openSync(''+path,''+mode,permissions))
	# } catch (e) {
	#	return builtin.error('' + e);
	# }	
	res.is_opened = true
	} $else {
		error('cannot open file on non NodeJS runtime')
	}
	return res
}
// open tries to open a file for reading and returns back a read-only `File` object.
pub fn open(path string) ?File {
	f := open_file(path,'r')?
	return f
}

pub fn create(path string) ?File {
	f := open_file(path,'w')?
	return f
}

pub fn stdin() File {
	return File {
		fd: 0,
		is_opened: true
	}
}

pub fn stdout() File {
	return File {
		fd: 1,
		is_opened: true
	}
}
pub fn stderr() File {
	return File {
		fd: 2,
		is_opened: true
	}
}

pub fn (f &File) read(mut buf []byte) ?int {
	
	if buf.len == 0 {
		return 0
	}
	mut nbytes := 0
	# try { 
	#	let buffer = $fs.readFileSync(f.fd.valueOf()); 
	#	
	#	for (const val of buffer.values()) { buf.arr[nbytes++] = val; }	
	# } 
	# catch (e) { return builtin.error('' + e); }
	return nbytes
}