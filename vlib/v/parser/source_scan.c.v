module parser

import v.token

// source_index_u8 returns the index of the first byte `c` of `src` at or after
// `start`, or -1 without one. The parser asks a few questions of a whole source
// before it parses it; this answers them at the speed of the C library's scan
// instead of a byte at a time.
@[inline]
fn source_index_u8(src string, c u8, start int) int {
	if start < 0 || start >= src.len {
		return -1
	}
	found := unsafe { &u8(C.memchr(src.str + start, c, uint(src.len - start))) }
	if isnil(found) {
		return -1
	}
	return int(unsafe { found - src.str })
}

// PrepassLineFile is the line table of the source that a const prepass reads,
// for the positions of pseudo variables in its comptime conditions.
struct PrepassLineFile {
	src  voidptr
	len  int
	path string
	file &token.File = unsafe { nil }
}

// BatchSources holds the sources of the files of one parallel parse batch, by
// the index of each file in the batch paths. The first pass of the batch reads
// them once for all of its passes, into memory of the C allocator, so the
// buffers outlive the disposable arenas of the pool tasks (see read_batch_source).
// Preloaded texts are borrowed from the caller instead and are not freed here.
@[heap]
struct BatchSources {
mut:
	sources  []string
	borrowed []bool
	index    map[string]int
	// The const names of the batch and before it, once the first pass found them.
	const_names     map[string]bool
	has_const_names bool
}

fn new_batch_sources(paths []string) &BatchSources {
	mut batch := &BatchSources{
		sources:  []string{len: paths.len}
		borrowed: []bool{len: paths.len}
	}
	for i, path in paths {
		if path !in batch.index {
			batch.index[path] = i
		}
	}
	return batch
}

// read reads the source of the file at `idx` of the batch, `path`. Tasks read
// distinct files, so each writes only its own slots.
fn (mut b BatchSources) read(idx int, path string) {
	b.sources[idx] = read_batch_source(path)
}

// preload borrows source text whose owner outlives every pass of this batch.
fn (mut b BatchSources) preload(idx int, source string) {
	b.sources[idx] = borrowed_source(source)
	b.borrowed[idx] = true
}

// source_at returns the source read for the file at `idx`, or none when the
// batch has not read it.
fn (b &BatchSources) source_at(idx int) ?string {
	if idx < 0 || idx >= b.sources.len || b.sources[idx].len == 0 {
		return none
	}
	return b.sources[idx]
}

// source_of returns the source read for the batch file `path`, or none.
fn (b &BatchSources) source_of(path string) ?string {
	idx := b.index[path] or { return none }
	return b.source_at(idx)
}

// release frees the sources of the files at [start, end) of the batch. Only
// call it once nothing refers to them any more.
fn (mut b BatchSources) release(start int, end int) {
	for i in start .. end {
		if b.sources[i].len > 0 {
			if !b.borrowed[i] {
				unsafe { C.free(b.sources[i].str) }
			}
			b.sources[i] = ''
		}
	}
}

// read_batch_source reads the regular file at `path` with plain system calls
// into memory of the C allocator, or returns '' when it cannot; the caller then
// reads the file the ordinary way, which reports the error. Every stream that
// fopen opens takes a process-wide lock, which the tasks of a parallel parse
// contended for.
fn read_batch_source(path string) string {
	$if windows {
		return ''
	} $else {
		fd := C.open(&char(path.str), C.O_RDONLY)
		if fd < 0 {
			return ''
		}
		mut st := C.stat{}
		if C.fstat(fd, &st) != 0 || u32(st.st_mode) & u32(C.S_IFMT) != u32(C.S_IFREG)
			|| i64(st.st_size) <= 0 {
			C.close(fd)
			return ''
		}
		size := i64(st.st_size)
		buf := unsafe { &u8(C.malloc(uint(size + 1))) }
		if isnil(buf) {
			C.close(fd)
			return ''
		}
		mut done := i64(0)
		for done < size {
			n := i64(C.read(fd, unsafe { buf + done }, uint(size - done)))
			if n <= 0 {
				break
			}
			done += n
		}
		C.close(fd)
		if done != size {
			unsafe { C.free(buf) }
			return ''
		}
		unsafe {
			buf[size] = 0
			return tos(buf, int(size))
		}
	}
}
