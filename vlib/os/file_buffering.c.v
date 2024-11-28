module os

fn C.setvbuf(stream &C.FILE, buffer &char, mode int, size usize) int

// FileBufferMode describes the available buffering modes for an os.File: unbuffered, line buffered and block/fully buffered.
// Normally all files are block buffered. If a stream refers to a terminal (as stdout normally does), it is line buffered.
// The standard error stream stderr is always unbuffered by default.
pub enum FileBufferMode {
	fully_buffered = C._IOFBF // many characters are saved up and written as a block
	line_buffered  = C._IOLBF // characters are saved up until a newline is output or input is read from any stream attached to a terminal device (typically stdin)
	not_buffered   = C._IONBF // information appears on the destination file or terminal as soon as it is written
}

// setvbuf sets the buffer and buffering mode for the given file `f`. See also os.FileBufferMode.
// It returns 0 on success. It returns nonzero on failure (the mode is invalid, or the request cannot be honored).
// Except for unbuffered files, the buffer argument should point to a buffer at least size bytes long; this buffer will be used instead of the current buffer.
// If the argument buffer is nil, only the mode is affected; a new buffer will be allocated on the next read or write operation.
// Note: make sure, that the space, that buffer points to (when != 0), still exists by the time the file stream is closed, which also happens at program termination.
// Note: f.setvbuf() may be used only after opening a file stream, and before any other operations have been performed on it.
@[unsafe]
pub fn (mut f File) setvbuf(buffer &char, mode FileBufferMode, size usize) int {
	return C.setvbuf(f.cfile, buffer, int(mode), size)
}

// set_buffer sets the buffer for the file, and the file buffering mode (see also os.FileBufferMode).
// Unlike File.setvbuf, it allows you to pass an existing V []u8 array directly.
// Note: f.set_buffer() may be used only after opening a file stream, and before any other operations have been performed on it.
pub fn (mut f File) set_buffer(mut buffer []u8, mode FileBufferMode) int {
	return unsafe { f.setvbuf(&char(buffer.data), mode, usize(buffer.len)) }
}

// set_line_buffered sets the file buffering mode to FileBufferMode.line_buffered.
// Note: f.set_line_buffered() may be used only after opening a file stream, and before any other operations have been performed on it.
pub fn (mut f File) set_line_buffered() {
	unsafe { f.setvbuf(&char(nil), .line_buffered, usize(0)) }
}

// set_unbuffered sets the file buffering mode to FileBufferMode.not_buffered.
// Note: f.set_unbuffered() may be used only after opening a file stream, and before any other operations have been performed on it.
pub fn (mut f File) set_unbuffered() {
	unsafe { f.setvbuf(&char(nil), .not_buffered, usize(0)) }
}
