module fastc

import os

fn C.setvbuf(stream &C.FILE, buffer &char, mode int, size uint) int

// write_c_pieces writes generated C pieces to `path` without joining them.
// Large outputs use a bounded buffer; write, final flush, and close errors propagate.
pub fn write_c_pieces(path string, pieces []string) ! {
	mut size := 0
	for piece in pieces {
		size += piece.len
		if size >= 1024 * 1024 {
			size = 1024 * 1024
			break
		}
	}
	mut buffer := []u8{len: if size >= 64 * 1024 { size } else { 0 }}
	file := os.vfopen(path, 'wb')!
	mut closed := false
	defer {
		if !closed {
			C.fclose(file)
		}
	}
	if buffer.len > 0 {
		// The V buffer remains alive until the stream is closed, including error paths.
		unsafe { C.setvbuf(file, &char(buffer.data), C._IOFBF, uint(buffer.len)) }
	}
	for piece in pieces {
		if piece.len > 0 && C.fwrite(piece.str, 1, piece.len, file) != piece.len {
			return os.error_posix()
		}
	}
	if C.fflush(file) != 0 {
		return os.error_posix()
	}
	closed = true
	if C.fclose(file) != 0 {
		return os.error_posix()
	}
}
