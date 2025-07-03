// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

import sync

// TODO: finish fileset / file / base pos etc

// compact encoding of a source position within a file set
pub type Pos = int

pub struct Position {
pub:
	filename string
	offset   int
	line     int
	column   int
}

pub fn (p Position) str() string {
	return '${p.filename}:${p.line}:${p.column}'
}

pub struct File {
pub:
	name string
	base int
	size int
mut:
	line_offsets []int = [0] // start of each line
}

pub struct FileSet {
mut:
	base int = 1 // reserve 0 for no position
	// files shared []&File
	files []&File
	mu    &sync.Mutex = sync.new_mutex()
}

pub fn FileSet.new() &FileSet {
	return &FileSet{}
}

// TODO:
pub fn (mut fs FileSet) add_file(filename string, base_ int, size int) &File {
	//	eprintln('>>> add_file fs: ${voidptr(fs)} | filename: $filename | base_: $base_ | size: $size')
	fs.mu.lock()
	defer {
		fs.mu.unlock()
	}
	mut base := if base_ < 0 { fs.base } else { base_ }

	// eprintln('>>> add_file fs: ${voidptr(fs)} | base: ${base:10} | fs.base: $fs.base | base_: ${base_:10} | size: ${size:10} | filename: $filename')

	if base < fs.base {
		panic('invalid base ${base} (should be >= ${fs.base}')
	}
	file := &File{
		name: filename
		base: base
		size: size
	}
	if size < 0 {
		panic('invalid size ${size} (should be >= 0)')
	}
	base += size + 1 // +1 because EOF also has a position
	if base < 0 {
		panic('token.Pos offset overflow (> 2G of source code in file set)')
	}
	// add the file to the file set
	// fs.base = base
	// lock fs.files {
	// 	// TODO: add shared to fs.base (fix compiler errors first)
	// 	fs.files << file
	// }
	// fs.last = file
	fs.base = base
	fs.files << file
	return file
}

fn search_files(files []&File, x int) int {
	// binary search
	mut min, mut max := 0, files.len
	for min < max {
		mid := (min + max) / 2
		// println('# min: $min, mid: $mid, max: $max')
		if files[mid].base <= x {
			min = mid + 1
		} else {
			max = mid
		}
	}
	return min - 1

	// linear search
	// for i := files.len-1; i>=0; i-- {
	// 	file := files[i]
	// 	if file.base < x && x <= file.base + file.size {
	// 		// println('found file for pos `$x` i = $i:')
	// 		// dump(file)
	// 		return i
	// 	}
	// }
	// return -1
}

pub fn (mut fs FileSet) file(pos Pos) &File {
	//	eprintln('>>>>>>>>> file fs: ${voidptr(fs)} | pos: $pos')
	fs.mu.lock()
	defer {
		fs.mu.unlock()
	}

	// lock fs.files
	// last file
	// if last_file := fs.files.last() {
	// 	// p_int
	// 	if last_file.base <= int(pos) && int(p) <- f.base+f.size {
	// 		return last_file
	// 	}
	// }
	// i := search_files(lock fs.files { fs.files }, pos)
	i := search_files(fs.files, pos)
	if i >= 0 {
		file := fs.files[i]
		if int(pos) <= file.base + file.size {
			// we could store last and retrieve and try above
			return file
		}
	}

	dump(fs)
	panic('cannot find file for pos: ${pos}')
}

// pub fn new_file(filename string) File {
// 	return File{
// 		name: filename
// 	}
// }

@[inline]
pub fn (mut f File) add_line(offset int) {
	f.line_offsets << offset
}

@[inline]
pub fn (f &File) line_count() int {
	return f.line_offsets.len
}

pub fn (f &File) line_start(line int) int {
	return f.line_offsets[line - 1] or {
		panic('invalid line `${line}` (must be > 0 & < ${f.line_count()})')
	}
}

pub fn (f &File) line(pos Pos) int {
	return f.find_line(pos)
}

pub fn (f &File) pos(offset int) Pos {
	if offset > f.size {
		panic('invalid offset')
	}
	return Pos(f.base + offset)
}

pub fn (f &File) position(pos Pos) Position {
	offset := int(pos) - f.base
	line, column := f.find_line_and_column(offset)
	return Position{
		filename: f.name
		offset:   offset
		line:     line
		column:   column
	}
}

// return (line, column) when passed pos
pub fn (f &File) find_line_and_column(pos int) (int, int) {
	line := f.find_line(pos)
	return line, pos - f.line_offsets[line - 1] + 1
}

// return line when passed pos (binary search)
// NOTE: only used for error conditions
// therefore search speed is not an issue
pub fn (f &File) find_line(pos int) int {
	mut min, mut max := 0, f.line_offsets.len
	for min < max {
		mid := (min + max) / 2
		// println('# min: $min, mid: $mid, max: $max')
		if f.line_offsets[mid] <= pos {
			min = mid + 1
		} else {
			max = mid
		}
	}
	return min
}
