module token

pub struct Pos {
pub:
	offset int
	id     int
}

pub fn (p Pos) str() string {
	return '{ offset: ${p.offset}, id: ${p.id} }'
}

pub fn (p Pos) is_valid() bool {
	return p.id > 0
}

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
	line_offsets []int = [0]
	id_counter   &int  = unsafe { nil }
}

pub struct FileSet {
mut:
	base       int = 1
	id_counter int
	files      []&File
}

pub fn FileSet.new() &FileSet {
	return &FileSet{}
}

pub fn (mut fs FileSet) add_file(filename string, base_ int, size int) &File {
	mut base := if base_ < 0 { fs.base } else { base_ }
	if base < fs.base {
		panic('invalid base ${base} (should be >= ${fs.base}')
	}
	file := &File{
		name:       filename
		base:       base
		size:       size
		id_counter: &fs.id_counter
	}
	if size < 0 {
		panic('invalid size ${size} (should be >= 0)')
	}
	base += size + 1
	if base < 0 {
		panic('token.Pos offset overflow (> 2G of source code in file set)')
	}
	fs.base = base
	fs.files << file
	return file
}

fn search_files(files []&File, x int) int {
	mut min, mut max := 0, files.len
	for min < max {
		mid := (min + max) / 2
		if files[mid].base <= x {
			min = mid + 1
		} else {
			max = mid
		}
	}
	return min - 1
}

pub fn (mut fs FileSet) file(pos Pos) &File {
	i := search_files(fs.files, pos.offset)
	if i >= 0 {
		file := fs.files[i]
		if pos.offset <= file.base + file.size {
			return file
		}
	}
	dump(fs)
	panic('cannot find file for pos: ${pos}')
}

@[inline]
pub fn (mut f File) add_line(offset int) {
	f.line_offsets << offset
}

@[inline]
pub fn (f &File) line_count() int {
	return f.line_offsets.len
}

pub fn (f &File) line_start(line int) int {
	idx := line - 1
	if idx < 0 || idx >= f.line_offsets.len {
		panic('invalid line `${line}` (must be > 0 & < ${f.line_count()})')
	}
	return f.line_offsets[idx]
}

pub fn (f &File) line(pos Pos) int {
	return f.find_line(pos.offset - f.base)
}

pub fn (mut f File) pos(offset int) Pos {
	if offset > f.size {
		panic('invalid offset')
	}
	mut current_id := 0
	mut next_id := 0
	unsafe {
		current_id = *f.id_counter
	}
	next_id = current_id + 1
	unsafe {
		*f.id_counter = next_id
	}
	return Pos{
		offset: f.base + offset
		id:     next_id
	}
}

pub fn (f &File) position(pos Pos) Position {
	offset := pos.offset - f.base
	line, column := f.find_line_and_column(offset)
	return Position{
		filename: f.name
		offset:   offset
		line:     line
		column:   column
	}
}

pub fn (f &File) find_line_and_column(pos int) (int, int) {
	line := f.find_line(pos)
	return line, pos - f.line_offsets[line - 1] + 1
}

pub fn (f &File) find_line(pos int) int {
	mut min, mut max := 0, f.line_offsets.len
	for min < max {
		mid := (min + max) / 2
		if f.line_offsets[mid] <= pos {
			min = mid + 1
		} else {
			max = mid
		}
	}
	return min
}
