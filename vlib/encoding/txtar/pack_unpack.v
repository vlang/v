module txtar

import os

// pack will create a txtar archive, given a path.
// When the path is a folder, it will walk over all files in that base folder, read their contents and create a File entry for each.
// When the path is a file, it will create an Archive, that contains just a single File entry, for that single file.
pub fn pack(path string, comment string) !Archive {
	if !os.exists(path) {
		return error('file or folder ${path} does not exist')
	}
	npath := path.replace(os.path_separator, '/')
	mut a := Archive{
		comment: comment
	}
	if os.is_file(npath) {
		fname := os.file_name(npath)
		fcontent := os.read_file(npath)!
		a.files << File{fname, fcontent}
		return a
	}
	files := os.walk_ext(npath, '').map(it.replace(os.path_separator, '/'))
	for f in files {
		frelative := f.replace_once(npath, '').trim_left('/')
		fcontent := os.read_file(f)!
		a.files << File{frelative, fcontent}
	}
	return a
}

// unpack will extract *all files* in the archive `a`, into the base folder `path`.
// Note that all file paths will be appended to the base folder `path`, i.e.
// if you have a File with `path` field == 'abc/def/x.v', and base folder path == '/tmp',
// then the final path for that File, will be '/tmp/abc/def/x.v'
// Note that unpack will try to create any of the intermediate folders like
// /tmp, /tmp/abc, /tmp/abc/def, if they do not already exist.
pub fn unpack(a &Archive, path string) ! {
	for f in a.files {
		full_path := os.join_path(path, f.path)
		folder := os.dir(full_path)
		if !os.exists(folder) {
			os.mkdir_all(folder)!
		}
		os.write_file(full_path, f.content)!
	}
}

// parse_file parses the given `file_path` as an archive.
// It will return an error, only if the `file_path` is not readable.
// See the README.md, or the test txtar_test.v, for a description of the format.
pub fn parse_file(file_path string) !Archive {
	content := os.read_file(file_path)!
	return parse(content)
}

// unpack_to extracts the content of the archive `a`, into the folder `path`.
pub fn (a &Archive) unpack_to(path string) ! {
	unpack(a, path)!
}
