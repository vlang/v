module token

// Pos represents pos data used by token.
pub struct Pos {
pub:
	offset int
	end    int
	id     int
}

// new_pos creates a source position from a stable file id and byte offset.
pub fn new_pos(file_id int, offset int) Pos {
	return Pos{
		id:     file_id
		offset: offset
		end:    offset
	}
}

// new_span creates an immutable half-open source span. The offset field is
// retained as the start for compatibility with existing diagnostic consumers.
pub fn new_span(file_id int, start int, end int) Pos {
	return Pos{
		id:     file_id
		offset: start
		end:    if end < start { start } else { end }
	}
}

// str returns the string form for Pos.
pub fn (p Pos) str() string {
	return '{ offset: ${p.offset}, end: ${p.end}, id: ${p.id} }'
}

// is_valid reports whether is valid applies in token.
pub fn (p Pos) is_valid() bool {
	return p.id > 0
}

// Position represents position data used by token.
pub struct Position {
pub:
	filename string
	offset   int
	line     int
	column   int
}

// str returns the string form for Position.
pub fn (p Position) str() string {
	return '${p.filename}:${p.line}:${p.column}'
}

// File represents file data used by token.
// Positions into a File use file-local byte offsets (Pos.offset), so a File no
// longer carries a FileSet-global base offset.
@[heap]
pub struct File {
pub:
	name string
	size int
mut:
	line_offsets []int = [0]
}

// FileSet represents file set data used by token.
pub struct FileSet {
mut:
	files []&File
}

// new creates a FileSet value for token.
pub fn FileSet.new() &FileSet {
	return &FileSet{}
}

// add_file registers a source file with the set. Under the file-local position
// representation there is no global base offset to assign; the caller keys the
// returned file by its stable Pos.id (e.g. FlatAst.source_files).
pub fn (mut fs FileSet) add_file(filename string, size int) &File {
	if size < 0 {
		panic('invalid size ${size} (should be >= 0)')
	}
	file := &File{
		name: filename
		size: size
	}
	fs.files << file
	return file
}

// add_line updates add line state for File.
@[inline]
pub fn (mut f File) add_line(offset int) {
	f.line_offsets << offset
}

// index_lines records every source-line start for logarithmic position lookup.
pub fn (mut f File) index_lines(src string) {
	f.line_offsets = [0]
	for i, ch in src {
		if ch == `\n` {
			f.line_offsets << i + 1
		}
	}
}

// line_count supports line count handling for File.
@[inline]
pub fn (f &File) line_count() int {
	return f.line_offsets.len
}

// line_start supports line start handling for File.
pub fn (f &File) line_start(line int) int {
	idx := line - 1
	if idx < 0 || idx >= f.line_offsets.len {
		panic('invalid line `${line}` (must be > 0 & < ${f.line_count()})')
	}
	return f.line_offsets[idx]
}

// line supports line handling for File.
// Pos.offset is already file-local (the safe constructors store the byte offset
// within this file, not a FileSet-global offset), so it is used directly.
pub fn (f &File) line(pos Pos) int {
	return f.find_line(pos.offset)
}

// position supports position handling for File.
// Pos.offset is file-local, matching position_at/FlatAst.source_position.
pub fn (f &File) position(pos Pos) Position {
	return f.position_at(pos.offset)
}

// position_at resolves a file-local byte offset to a presentation position.
pub fn (f &File) position_at(offset int) Position {
	line, column := f.find_line_and_column(offset)
	return Position{
		filename: f.name
		offset:   offset
		line:     line
		column:   column
	}
}

// find_line_and_column resolves find line and column information for token.
pub fn (f &File) find_line_and_column(pos int) (int, int) {
	line := f.find_line(pos)
	return line, pos - f.line_offsets[line - 1] + 1
}

// find_line resolves find line information for token.
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
