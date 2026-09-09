module fs

import io
import os

// FileInfo describes a file or directory entry.
pub interface FileInfo {
	name() string
	size() u64
	mode() os.FileMode
	mod_time() i64
	is_dir() bool
}

// DirEntry describes a single directory entry.
pub interface DirEntry {
	name() string
	is_dir() bool
	typ() os.FileType
	info() !FileInfo
}

// File is the minimum interface required to read from a filesystem entry.
pub interface File {
	io.Reader
	stat() !FileInfo
mut:
	close()
}

// FS opens slash-separated paths that pass `valid_path`.
pub interface FS {
	open(name string) !File
}

// ReadDirFile can read directory entries directly from an opened file.
pub interface ReadDirFile {
	File
mut:
	read_dir(n int) ![]DirEntry
}

// ReadDirFS can read a directory without first opening it as a `File`.
pub interface ReadDirFS {
	read_dir(name string) ![]DirEntry
}

// ReadFileFS can read a whole file without first opening it as a `File`.
pub interface ReadFileFS {
	read_file(name string) ![]u8
}

// StatFS can stat a path without first opening it as a `File`.
pub interface StatFS {
	stat(name string) !FileInfo
}

// GlobFS can expand glob patterns within a filesystem.
pub interface GlobFS {
	glob(pattern string) ![]string
}

// SubFS can return a filesystem rooted at a subdirectory.
pub interface SubFS {
	sub(dir string) !FS
}

struct FileInfoDirEntry {
	source FileInfo
}

fn (entry FileInfoDirEntry) name() string {
	return entry.source.name()
}

fn (entry FileInfoDirEntry) is_dir() bool {
	return entry.source.is_dir()
}

fn (entry FileInfoDirEntry) typ() os.FileType {
	return entry.source.mode().typ
}

fn (entry FileInfoDirEntry) info() !FileInfo {
	return entry.source
}

fn sort_dir_entries(mut entries []DirEntry) {
	entries.sort(a.name() < b.name())
}

// valid_path reports whether `name` is a valid slash-separated path for `FS.open`.
pub fn valid_path(name string) bool {
	if name == '.' {
		return true
	}
	if name.len == 0 {
		return false
	}
	for elem in name.split('/') {
		if elem.len == 0 || elem == '.' || elem == '..' {
			return false
		}
	}
	return true
}

// file_info_to_dir_entry wraps a `FileInfo` in a `DirEntry`.
pub fn file_info_to_dir_entry(info FileInfo) DirEntry {
	return FileInfoDirEntry{
		source: info
	}
}

// read_file reads the named file from `filesystem`.
pub fn read_file(filesystem FS, name string) ![]u8 {
	if filesystem is ReadFileFS {
		reader := filesystem as ReadFileFS
		return reader.read_file(name)
	}
	mut file := filesystem.open(name)!
	defer {
		file.close()
	}
	reader := file as io.Reader
	return io.read_all(
		reader:                reader
		read_to_end_of_stream: true
	)
}

// read_dir reads and sorts the named directory from `filesystem`.
pub fn read_dir(filesystem FS, name string) ![]DirEntry {
	if filesystem is ReadDirFS {
		reader := filesystem as ReadDirFS
		mut entries := reader.read_dir(name)!
		sort_dir_entries(mut entries)
		return entries
	}
	mut file := filesystem.open(name)!
	defer {
		file.close()
	}
	if mut file is ReadDirFile {
		mut entries := file.read_dir(-1)!
		sort_dir_entries(mut entries)
		return entries
	}
	return error('fs: `${name}` does not support read_dir')
}

// stat returns file information for the named path from `filesystem`.
pub fn stat(filesystem FS, name string) !FileInfo {
	if filesystem is StatFS {
		stats := filesystem as StatFS
		return stats.stat(name)
	}
	mut file := filesystem.open(name)!
	defer {
		file.close()
	}
	return file.stat()
}
