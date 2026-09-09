module fs

import io
import os

struct MockInfo {
	name_     string
	size_     u64
	mode_     os.FileMode
	mod_time_ i64
}

fn (info MockInfo) name() string {
	return info.name_
}

fn (info MockInfo) size() u64 {
	return info.size_
}

fn (info MockInfo) mode() os.FileMode {
	return info.mode_
}

fn (info MockInfo) mod_time() i64 {
	return info.mod_time_
}

fn (info MockInfo) is_dir() bool {
	return info.mode_.typ == .directory
}

struct MockFile {
	data  []u8
	info_ MockInfo
mut:
	offset  int
	entries []DirEntry
}

fn (mut file MockFile) read(mut buf []u8) !int {
	if file.offset >= file.data.len {
		return io.Eof{}
	}
	read := copy(mut buf, file.data[file.offset..])
	file.offset += read
	return read
}

fn (file MockFile) stat() !FileInfo {
	return file.info_
}

fn (mut file MockFile) close() {}

fn (mut file MockFile) read_dir(n int) ![]DirEntry {
	if n <= 0 || n >= file.entries.len {
		return file.entries.clone()
	}
	return file.entries[..n].clone()
}

struct FallbackFS {}

fn (filesystem FallbackFS) open(name string) !File {
	match name {
		'notes.txt' {
			return File(MockFile{
				data:  'fallback'.bytes()
				info_: MockInfo{
					name_:     'notes.txt'
					size_:     u64('fallback'.len)
					mode_:     os.FileMode{
						typ: .regular
					}
					mod_time_: 101
				}
			})
		}
		'dir' {
			return File(MockFile{
				info_:   MockInfo{
					name_: 'dir'
					mode_: os.FileMode{
						typ: .directory
					}
				}
				entries: [
					file_info_to_dir_entry(MockInfo{
						name_: 'b.txt'
						mode_: os.FileMode{
							typ: .regular
						}
					}),
					file_info_to_dir_entry(MockInfo{
						name_: 'a.txt'
						mode_: os.FileMode{
							typ: .regular
						}
					}),
				]
			})
		}
		else {
			return error('missing ${name}')
		}
	}
}

struct OptimizedFS {}

fn (filesystem OptimizedFS) open(name string) !File {
	return error('unexpected open for ${name}')
}

fn (filesystem OptimizedFS) read_file(name string) ![]u8 {
	return 'optimized'.bytes()
}

fn (filesystem OptimizedFS) read_dir(name string) ![]DirEntry {
	return [
		file_info_to_dir_entry(MockInfo{
			name_: 'z.txt'
			mode_: os.FileMode{
				typ: .regular
			}
		}),
		file_info_to_dir_entry(MockInfo{
			name_: 'm.txt'
			mode_: os.FileMode{
				typ: .regular
			}
		}),
	]
}

fn (filesystem OptimizedFS) stat(name string) !FileInfo {
	return MockInfo{
		name_:     name
		size_:     42
		mode_:     os.FileMode{
			typ: .regular
		}
		mod_time_: 202
	}
}

fn entry_names(entries []DirEntry) []string {
	mut names := []string{cap: entries.len}
	for entry in entries {
		names << entry.name()
	}
	return names
}

fn test_valid_path() {
	assert valid_path('.')
	assert valid_path('alpha')
	assert valid_path('alpha/beta')
	assert valid_path(r'alpha\beta')
	assert !valid_path('')
	assert !valid_path('/alpha')
	assert !valid_path('alpha/')
	assert !valid_path('alpha//beta')
	assert !valid_path('alpha/./beta')
	assert !valid_path('alpha/../beta')
}

fn test_file_info_to_dir_entry() {
	info := MockInfo{
		name_:     'subdir'
		size_:     12
		mode_:     os.FileMode{
			typ: .directory
		}
		mod_time_: 303
	}
	entry := file_info_to_dir_entry(info)
	assert entry.name() == 'subdir'
	assert entry.is_dir()
	assert entry.typ() == .directory
	assert entry.info()!.mod_time() == 303
}

fn test_read_file_uses_open_fallback() {
	data := read_file(FallbackFS{}, 'notes.txt')!
	assert data == 'fallback'.bytes()
}

fn test_read_file_uses_read_file_fs_when_available() {
	data := read_file(OptimizedFS{}, 'notes.txt')!
	assert data == 'optimized'.bytes()
}

fn test_read_dir_uses_open_fallback_and_sorts() {
	entries := read_dir(FallbackFS{}, 'dir')!
	assert entry_names(entries) == ['a.txt', 'b.txt']
}

fn test_read_dir_uses_read_dir_fs_when_available() {
	entries := read_dir(OptimizedFS{}, 'dir')!
	assert entry_names(entries) == ['m.txt', 'z.txt']
}

fn test_stat_uses_open_fallback() {
	info := stat(FallbackFS{}, 'notes.txt')!
	assert info.name() == 'notes.txt'
	assert info.size() == u64('fallback'.len)
}

fn test_stat_uses_stat_fs_when_available() {
	info := stat(OptimizedFS{}, 'notes.txt')!
	assert info.name() == 'notes.txt'
	assert info.size() == 42
	assert info.mod_time() == 202
}
