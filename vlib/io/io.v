module io

// CopySettings provides additional options to io.cp
@[params]
pub struct CopySettings {
pub mut:
	buffer_size int = 64 * 1024 // The buffer size used during the copying. A larger buffer is more performant, but uses more RAM.
}

// cp copies from `src` to `dst` by allocating
// a maximum of 1024 bytes buffer for reading
// until either EOF is reached on `src` or an error occurs.
// An error is returned if an error is encountered during write.
@[manualfree]
pub fn cp(mut src Reader, mut dst Writer, params CopySettings) ! {
	mut buf := []u8{len: params.buffer_size}
	defer {
		unsafe {
			buf.free()
		}
	}
	for {
		bytes := src.read(mut buf) or { break }
		dst.write(buf[..bytes]) or { return err }
	}
}
