// No type in this program implements `fs.ReadFileFS`, `fs.ReadDirFS` or `fs.StatFS`,
// so `fs.read_file`, `fs.read_dir` and `fs.stat` always take their `open` fallbacks.
import io
import io.fs
import os

struct TestInfo {
	name_ string
	size_ u64
	dir   bool
}

fn (info TestInfo) name() string {
	return info.name_
}

fn (info TestInfo) size() u64 {
	return info.size_
}

fn (info TestInfo) mode() os.FileMode {
	return os.FileMode{
		typ: if info.dir { .directory } else { .regular }
	}
}

fn (info TestInfo) mod_time() i64 {
	return 7
}

fn (info TestInfo) is_dir() bool {
	return info.dir
}

struct TestFile {
	data []u8
	info TestInfo
mut:
	offset int
}

fn (mut file TestFile) read(mut buf []u8) !int {
	if file.offset >= file.data.len {
		return io.Eof{}
	}
	n := copy(mut buf, file.data[file.offset..])
	file.offset += n
	return n
}

fn (file TestFile) stat() !fs.FileInfo {
	return file.info
}

fn (mut file TestFile) close() {}

struct TestDir {
	info TestInfo
}

fn (dir TestDir) read(mut buf []u8) !int {
	return io.Eof{}
}

fn (dir TestDir) stat() !fs.FileInfo {
	return dir.info
}

fn (mut dir TestDir) close() {}

fn (mut dir TestDir) read_dir(n int) ![]fs.DirEntry {
	return [
		fs.file_info_to_dir_entry(TestInfo{
			name_: 'b.txt'
		}),
		fs.file_info_to_dir_entry(TestInfo{
			name_: 'a.txt'
		}),
	]
}

// OpenOnlyFS implements only `fs.FS`.
struct OpenOnlyFS {}

fn (filesystem OpenOnlyFS) open(name string) !fs.File {
	match name {
		'notes.txt' {
			return fs.File(TestFile{
				data: 'hello'.bytes()
				info: TestInfo{
					name_: name
					size_: 5
				}
			})
		}
		'dir' {
			return fs.File(TestDir{
				info: TestInfo{
					name_: name
					dir:   true
				}
			})
		}
		else {
			return error('missing ${name}')
		}
	}
}

fn test_read_file_opens_the_file() {
	assert fs.read_file(OpenOnlyFS{}, 'notes.txt')!.bytestr() == 'hello'
	if _ := fs.read_file(OpenOnlyFS{}, 'missing.txt') {
		assert false
	} else {
		assert err.msg() == 'missing missing.txt'
	}
}

fn test_read_dir_opens_the_directory() {
	entries := fs.read_dir(OpenOnlyFS{}, 'dir')!
	assert entries.map(it.name()) == ['a.txt', 'b.txt']
	if _ := fs.read_dir(OpenOnlyFS{}, 'notes.txt') {
		assert false
	} else {
		assert err.msg() == 'fs: `notes.txt` does not support read_dir'
	}
}

fn test_stat_opens_the_file() {
	info := fs.stat(OpenOnlyFS{}, 'notes.txt')!
	assert info.name() == 'notes.txt'
	assert info.size() == 5
	assert !info.is_dir()
	assert fs.stat(OpenOnlyFS{}, 'dir')!.is_dir()
}
