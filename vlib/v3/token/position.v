module token

import crypto.sha256

// Pos represents pos data used by token.
pub struct Pos {
pub:
	offset int
	end    int
	id     int
	meta   u16
}

const type_text_meta_flag = u16(0x8000)

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

// with_reported_column overrides the one-based column shown in a diagnostic header.
// Columns that do not fit in the compact metadata are ignored so diagnostics can
// fall back to the source position instead of aborting compilation.
pub fn (p Pos) with_reported_column(column int) Pos {
	if column < 0 || column >= int(type_text_meta_flag) {
		return p
	}
	return Pos{
		...p
		meta: u16(column)
	}
}

// reported_column returns the optional diagnostic column override. The high
// meta bit belongs to Node's canonical type-text id and is invisible to
// ordinary source-position consumers.
@[inline]
pub fn (p Pos) reported_column() int {
	return if p.meta & type_text_meta_flag == 0 { int(p.meta) } else { 0 }
}

// type_text_id returns the compact Node annotation identity stored in unused
// position metadata. Source positions carrying a diagnostic column do not
// carry a type identity.
@[inline]
pub fn (p Pos) type_text_id() u16 {
	return if p.meta & type_text_meta_flag != 0 { p.meta & ~type_text_meta_flag } else { 0 }
}

// with_type_text_id stores a compact identity when no diagnostic column is
// present. Larger ids use the existing string-keyed type cache.
@[inline]
pub fn (p Pos) with_type_text_id(id u16) Pos {
	if p.reported_column() > 0 {
		return p
	}
	return Pos{
		...p
		meta: if id > 0 && id < type_text_meta_flag { type_text_meta_flag | id } else { 0 }
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
	// Keep the digest inline: stored files can outlive parser-worker preallocation scopes.
	source_digest     [sha256.size]u8
	has_source_digest bool
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

// File.unindexed creates a file record for scanners whose diagnostics report
// raw byte offsets and never resolve line positions (FastC). It allocates only
// the record itself and leaves the line table empty, so such a file must not
// be used for position lookups.
pub fn File.unindexed(name string, size int) &File {
	return &File{
		name:         name
		size:         size
		line_offsets: []int{}
	}
}

// add_line updates add line state for File.
@[inline]
pub fn (mut f File) add_line(offset int) {
	f.line_offsets << offset
}

// index_lines records every source-line start for logarithmic position lookup
// and stores the source digest consumed by cache and fallback verification.
pub fn (mut f File) index_lines(src string) {
	f.index_lines_without_digest(src)
	for i, c in src {
		if c == `\n` {
			f.line_offsets << i + 1
		}
	}
	digest := sha256.sum(src.bytes())
	for i in 0 .. sha256.size {
		f.source_digest[i] = digest[i]
	}
	f.has_source_digest = true
}

// index_lines_without_digest resets the line table without indexing. FastC is
// its only caller: it builds a fresh file per source file for every collection
// and generation pass, its scanner reads `src` directly, and its diagnostics
// report raw byte offsets rather than line/column positions. Building the
// per-line offset table (and the growing []int backing it) on every pass was
// pure overhead, so the table is left empty; a position lookup on such a file
// would resolve to line 1, which FastC never requests. The unused `src`
// parameter keeps the call sites and signature stable.
pub fn (mut f File) index_lines_without_digest(src string) {
	_ := src
	f.line_offsets = [0]
	f.has_source_digest = false
}

// source_sha256 returns the digest bytes of the exact source indexed for this file.
pub fn (f &File) source_sha256() [sha256.size]u8 {
	return f.source_digest
}

// has_source_sha256 reports whether this file index was built from source bytes.
pub fn (f &File) has_source_sha256() bool {
	return f.has_source_digest
}

// set_source_sha256 preserves a source digest when a parser worker clones a file index.
pub fn (mut f File) set_source_sha256(digest [sha256.size]u8) {
	f.source_digest = digest
	f.has_source_digest = true
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
