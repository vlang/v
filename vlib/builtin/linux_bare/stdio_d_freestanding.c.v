@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_stdio_abi.h"
	#include <fcntl.h>
	#include <sys/syscall.h>
	#include <errno.h>

	fn C.v_stdio_standard(i32) uint
	fn C.v_stdio_stdout() uint
	fn C.v_stdio_standard_size() uint
	fn C.v_stdio_cookie_read(uint, uint, uint, uint) i64
	fn C.v_stdio_cookie_write(uint, uint, uint, uint) i64
	fn C.v_stdio_cookie_seek(uint, uint, &i64, i32) i32
	fn C.v_stdio_cookie_close(uint, uint) i32
	fn C.fcntl(i32, i32, ...voidptr) i32
	fn C.isatty(i32) i32
	fn C.mkostemp(&char, i32) i32
	fn C.v_stdio_decode(uint, uint, uint, i32) uint
	fn C.v_stdio_encode(uint, u32, uint, i32) uint
	fn C.v_stdio_position_set(uint, i64, uint)
	fn C.v_stdio_position_get(uint, uint) i64

	@[heap]
	struct BareStream {
	mut:
		// GNU header inlines access flags and four buffer pointers in this prefix.
		abi_flags      i32
		abi_storage    [52]u8
		lock_word      u32
		owner          uint
		depth          u32
		fd             i32
		allocated      bool
		mode           i32
		direction      u32
		append         bool
		readable       bool
		writable       bool
		closing        bool
		references     u32
		identifier     u64
		buffer         &u8 = unsafe { nil }
		capacity       uint
		read_at        uint
		read_end       uint
		write_at       uint
		owns_buffer    bool
		pushed         [16]u8
		pushed_count   uint
		next           &BareStream = unsafe { nil }
		is_cookie      bool
		writing_cookie bool
		cookie         uint
		callbacks      [4]uint
		orientation    i32
		utf8           i32
		read_state     [8]u8
		write_state    [8]u8
	}

	__global bare_streams_lock u32
	__global bare_streams_ready u32
	__global bare_streams_first &BareStream
	__global bare_streams_identifier u64

	fn bare_stdio_init() {
		if bare_load_u32(&bare_streams_ready) != 0 { return }
		bare_compat_lock(&bare_streams_lock)
		defer { bare_compat_unlock(&bare_streams_lock) }
		if bare_load_u32(&bare_streams_ready) != 0 { return }
		// Standard streams have the same ABI prefix and private state as heap streams.
		if sizeof(BareStream) > C.v_stdio_standard_size() { bare_thread_fail() }
		for i in 0 .. 3 {
			mut stream := unsafe { &BareStream(C.v_stdio_standard(i32(i))) }
			stream.fd = i32(i)
			stream.mode = if i == 2 { i32(C._IONBF) } else { i32(-1) }
			stream.readable = i == 0
			stream.writable = i != 0
			bare_streams_identifier++
			stream.identifier = bare_streams_identifier
			stream.next = bare_streams_first
			bare_streams_first = stream
		}
		bare_store_u32(&bare_streams_ready, 1)
	}
	fn bare_stream(address uint) &BareStream {
		bare_stdio_init()
		return unsafe { &BareStream(address) }
	}
	@[export: 'v_stdio_lock'; markused]
	fn bare_stdio_lock(address uint) {
		mut stream := bare_stream(address)
		owner := C.v_pthread_tls_identity()
		if bare_compat_load_uint(&stream.owner) == owner {
			stream.depth++
			return
		}
		bare_compat_lock(&stream.lock_word)
		bare_compat_store_uint(&stream.owner, owner)
		stream.depth = 1
	}
	@[export: 'v_stdio_trylock'; markused]
	fn bare_stdio_trylock(address uint) i32 {
		mut stream := bare_stream(address)
		owner := C.v_pthread_tls_identity()
		if bare_compat_load_uint(&stream.owner) == owner {
			stream.depth++
			return 0
		}
		if !bare_cas_u32(&stream.lock_word, 0, 1) { return 1 }
		bare_compat_store_uint(&stream.owner, owner)
		stream.depth = 1
		return 0
	}
	@[export: 'v_stdio_unlock'; markused]
	fn bare_stdio_unlock(address uint) {
		mut stream := unsafe { &BareStream(address) }
		stream.depth--
		if stream.depth != 0 { return }
		bare_compat_store_uint(&stream.owner, 0)
		bare_compat_unlock(&stream.lock_word)
	}
	fn bare_stream_fail(mut stream BareStream, error i32) i32 {
		stream.abi_flags |= i32(C.V_STDIO_ERROR)
		bf_native_errno_cell = error
		return -1
	}
	fn bare_stream_buffer(mut stream BareStream) bool {
		if stream.mode == -1 {
			saved_errno := bf_native_errno_cell
			stream.mode = if C.isatty(stream.fd) != 0 { i32(C._IOLBF) } else { i32(C._IOFBF) }
			bf_native_errno_cell = saved_errno
		}
		if stream.mode == i32(C._IONBF) || stream.buffer != unsafe { nil } { return true }
		stream.buffer = unsafe { &u8(__malloc(uint(C.BUFSIZ))) }
		if stream.buffer == unsafe { nil } {
			bare_stream_fail(mut stream, i32(C.ENOMEM))
			return false
		}
		stream.capacity = uint(C.BUFSIZ)
		stream.owns_buffer = true
		return true
	}
	fn bare_stream_raw_read(mut stream BareStream, output uint, size uint) i64 {
		if stream.mode == i32(C._IONBF) || stream.mode == i32(C._IOLBF) {
			// Match the input-to-stdout lock order at refill, leaving buffered reads alone.
			address := C.v_stdio_stdout()
			bare_stdio_lock(address)
			mut out := unsafe { &BareStream(address) }
			if (out.fd >= 0 || out.is_cookie) && out.writable && !out.writing_cookie && out.mode == i32(C._IOLBF) && out.write_at != 0 {
				saved_errno := bf_native_errno_cell
				bare_stream_flush(mut out)
				bf_native_errno_cell = saved_errno
			}
			bare_stdio_unlock(address)
		}
		if !stream.is_cookie {
			return i64(sys_call3(bf_nr_read, u64(stream.fd), u64(output), u64(size)))
		}
		if stream.callbacks[0] == 0 { return 0 }
		count := C.v_stdio_cookie_read(stream.callbacks[0], stream.cookie, output, size)
		if count < 0 {
			return -i64(if bf_native_errno_cell != 0 { bf_native_errno_cell } else { i32(C.EIO) })
		}
		return if uint(count) <= size { count } else { -i64(C.EIO) }
	}
	fn bare_stream_seek(mut stream BareStream, offset i64, whence i32) i64 {
		if !stream.is_cookie {
			return bare_native_result(sys_call3(bf_nr_lseek, u64(stream.fd), u64(offset), u64(whence)))
		}
		if stream.callbacks[2] == 0 {
			bf_native_errno_cell = i32(C.ESPIPE)
			return -1
		}
		mut position := offset
		if C.v_stdio_cookie_seek(stream.callbacks[2], stream.cookie, &position, whence) != 0 {
			return -1
		}
		if position < 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		return position
	}
	fn bare_stream_raw_write(mut stream BareStream, input uint, size uint) i64 {
		if !stream.is_cookie {
			return i64(sys_call3(bf_nr_write, u64(stream.fd), u64(input), u64(size)))
		}
		if stream.callbacks[1] == 0 { return i64(size) }
		// A cookie write/seek callback can refill another input stream.
		// Its implicit stdout flush must not send these same bytes again.
		was_writing := stream.writing_cookie
		stream.writing_cookie = true
		defer { stream.writing_cookie = was_writing }
		if stream.append && stream.callbacks[2] != 0 && bare_stream_seek(mut stream, 0, 2) < 0 {
			return -i64(bf_native_errno_cell)
		}
		count := C.v_stdio_cookie_write(stream.callbacks[1], stream.cookie, input, size)
		if count < 0 || (count == 0 && size != 0) {
			return -i64(if bf_native_errno_cell != 0 { bf_native_errno_cell } else { i32(C.EIO) })
		}
		return if uint(count) <= size { count } else { -i64(C.EIO) }
	}
	fn bare_stream_flush(mut stream BareStream) i32 {
		if stream.direction == 1 && (stream.read_end != stream.read_at || stream.pushed_count != 0) {
			unread := stream.read_end - stream.read_at + stream.pushed_count
			if bare_stream_seek(mut stream, -i64(unread), 1) < 0 {
				return bare_stream_fail(mut stream, bf_native_errno_cell)
			}
			stream.read_at = 0
			stream.read_end = 0
			stream.pushed_count = 0
		}
		mut done := uint(0)
		for done < stream.write_at {
			count := bare_stream_raw_write(mut stream, uint(stream.buffer) + done, stream.write_at - done)
			if count <= 0 {
				if done != 0 {
					unsafe { vmemmove(stream.buffer, stream.buffer + done, int(stream.write_at - done)) }
				}
				stream.write_at -= done
				return bare_stream_fail(mut stream, if count < 0 { i32(-count) } else { i32(C.EIO) })
			}
			done += uint(count)
		}
		stream.write_at = 0
		return 0
	}
	fn bare_stream_mode(mode uint) (i32, bool) {
		if mode == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return 0, false
		}
		text := unsafe { tos2(&u8(mode)) }
		if text.len == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return 0, false
		}
		mut flags := match text[0] {
			`r` { i32(C.O_RDONLY) }
			`w` { i32(C.O_WRONLY) | i32(C.O_CREAT) | i32(C.O_TRUNC) }
			`a` { i32(C.O_WRONLY) | i32(C.O_CREAT) | i32(C.O_APPEND) }
			else {
				bf_native_errno_cell = i32(C.EINVAL)
				return 0, false
			}
		}
		for i in 1 .. text.len {
			match text[i] {
				`+` { flags = (flags & ~i32(C.O_ACCMODE)) | i32(C.O_RDWR) }
				`e` { flags |= i32(C.O_CLOEXEC) }
				`x` { flags |= i32(C.O_EXCL) }
				`b` {}
				else {
					bf_native_errno_cell = i32(C.EINVAL)
					return 0, false
				}
			}
		}
		return flags, true
	}
	fn bare_stream_new(fd i32, flags i32) &BareStream {
		mut stream := unsafe { &BareStream(__calloc(1, sizeof(BareStream))) }
		if stream == unsafe { nil } { return stream }
		stream.fd = fd
		stream.mode = if fd < 0 { i32(C._IOFBF) } else { i32(-1) }
		stream.allocated = true
		stream.readable = flags & i32(C.O_ACCMODE) != i32(C.O_WRONLY)
		stream.writable = flags & i32(C.O_ACCMODE) != i32(C.O_RDONLY)
		stream.append = flags & i32(C.O_APPEND) != 0
		bare_compat_lock(&bare_streams_lock)
		bare_streams_identifier++
		stream.identifier = bare_streams_identifier
		stream.next = bare_streams_first
		bare_streams_first = stream
		bare_compat_unlock(&bare_streams_lock)
		return stream
	}
	@[export: 'v_stdio_fdopen'; markused]
	fn bare_stdio_fdopen(fd i32, mode uint) uint {
		bare_stdio_init()
		flags, valid := bare_stream_mode(mode)
		if !valid { return 0 }
		existing := C.fcntl(fd, i32(C.F_GETFL))
		if existing < 0 { return 0 }
		if existing & i32(C.O_ACCMODE) != i32(C.O_RDWR) && existing & i32(C.O_ACCMODE) != flags & i32(C.O_ACCMODE) {
			bf_native_errno_cell = i32(C.EINVAL)
			return 0
		}
		if flags & i32(C.O_APPEND) != 0 && existing & i32(C.O_APPEND) == 0 && C.fcntl(fd, i32(C.F_SETFL), existing | i32(C.O_APPEND)) < 0 {
			return 0
		}
		if flags & i32(C.O_CLOEXEC) != 0 && C.fcntl(fd, i32(C.F_SETFD), i32(C.FD_CLOEXEC)) < 0 {
			return 0
		}
		return uint(bare_stream_new(fd, flags))
	}
	@[export: 'v_stdio_fopen'; markused]
	fn bare_stdio_fopen(path uint, mode uint) uint {
		bare_stdio_init()
		flags, valid := bare_stream_mode(mode)
		if !valid { return 0 }
		fd := bare_native_openat(-100, path, flags, 0o666)
		if fd < 0 { return 0 }
		stream := bare_stream_new(fd, flags)
		if stream == unsafe { nil } {
			error := bf_native_errno_cell
			C.close(fd)
			bf_native_errno_cell = error
		}
		return uint(stream)
	}
	@[export: 'v_stdio_cookie'; markused]
	fn bare_stdio_cookie(cookie uint, mode uint, callbacks uint) uint {
		bare_stdio_init()
		flags, valid := bare_stream_mode(mode)
		if !valid { return 0 }
		mut stream := bare_stream_new(-1, flags)
		if stream == unsafe { nil } { return 0 }
		stream.cookie = cookie
		for i in 0 .. 4 { stream.callbacks[i] = unsafe { (&uint(callbacks))[i] } }
		stream.is_cookie = true
		return uint(stream)
	}
	@[export: 'v_stdio_flush'; markused]
	fn bare_stdio_flush(address uint) i32 {
		bare_stdio_init()
		if address != 0 {
			bare_stdio_lock(address)
			mut stream := unsafe { &BareStream(address) }
			result := bare_stream_flush(mut stream)
			bare_stdio_unlock(address)
			return result
		}
		bare_compat_lock(&bare_streams_lock)
		limit := bare_streams_identifier
		bare_compat_unlock(&bare_streams_lock)
		mut previous := u64(0)
		mut result := i32(0)
		mut error := i32(0)
		for {
			bare_compat_lock(&bare_streams_lock)
			mut chosen := unsafe { &BareStream(nil) }
			mut entry := bare_streams_first
			for entry != unsafe { nil } {
				if entry.identifier > previous && entry.identifier <= limit && (chosen == unsafe { nil } || entry.identifier < chosen.identifier) {
					chosen = entry
				}
				entry = entry.next
			}
			if chosen == unsafe { nil } {
				bare_compat_unlock(&bare_streams_lock)
				break
			}
			previous = chosen.identifier
			chosen.references++
			bare_compat_unlock(&bare_streams_lock)
			bare_stdio_lock(uint(chosen))
			if (chosen.fd >= 0 || chosen.is_cookie) && chosen.direction == 2 && bare_stream_flush(mut chosen) < 0 {
				result = -1
				error = bf_native_errno_cell
			}
			bare_stdio_unlock(uint(chosen))
			bare_compat_lock(&bare_streams_lock)
			chosen.references--
			if chosen.references == 0 && chosen.closing && chosen.allocated {
				unsafe { __free(chosen) }
			}
			bare_compat_unlock(&bare_streams_lock)
		}
		if result != 0 { bf_native_errno_cell = error }
		return result
	}
	@[export: 'v_freestanding_stdio_flush'; markused]
	fn bare_stdio_exit_flush() { bare_stdio_flush(0) }
	@[export: 'v_stdio_close'; markused]
	fn bare_stdio_close(address uint) i32 {
		bare_stdio_lock(address)
		mut stream := unsafe { &BareStream(address) }
		saved_errno := bf_native_errno_cell
		mut result := if stream.is_cookie && stream.direction == 1 {
			i32(0)
		} else {
			bare_stream_flush(mut stream)
		}
		if result < 0 && stream.direction == 1 && bf_native_errno_cell == i32(C.ESPIPE) {
			// A nonseekable reader can discard read-ahead on close.
			result = 0
			bf_native_errno_cell = saved_errno
		}
		mut error := bf_native_errno_cell
		closed := if stream.is_cookie {
			if stream.callbacks[3] != 0 {
				C.v_stdio_cookie_close(stream.callbacks[3], stream.cookie)
			} else {
				i32(0)
			}
		} else {
			C.close(stream.fd)
		}
		if closed != 0 && result == 0 {
			result = -1
			error = bf_native_errno_cell
		}
		if stream.owns_buffer { unsafe { __free(stream.buffer) } }
		stream.fd = -1
		stream.is_cookie = false
		bare_stdio_unlock(address)
		bare_compat_lock(&bare_streams_lock)
		stream.closing = true
		mut previous := unsafe { &BareStream(nil) }
		mut at := bare_streams_first
		for at != unsafe { nil } && at != stream {
			previous = at
			at = at.next
		}
		if at != unsafe { nil } {
			if previous == unsafe { nil } {
				bare_streams_first = stream.next
			} else {
				previous.next = stream.next
			}
		}
		if stream.allocated && stream.references == 0 { unsafe { __free(stream) } }
		bare_compat_unlock(&bare_streams_lock)
		if result != 0 { bf_native_errno_cell = error }
		return result
	}
	@[export: 'v_stdio_read'; markused]
	fn bare_stdio_read(output uint, size uint, count uint, address uint) uint {
		mut stream := bare_stream(address)
		if size == 0 || count == 0 { return 0 }
		if !stream.readable {
			bare_stream_fail(mut stream, i32(C.EBADF))
			return 0
		}
		if count > uint(max_int) / size {
			bare_stream_fail(mut stream, i32(C.EOVERFLOW))
			return 0
		}
		if stream.orientation == 0 { stream.orientation = -1 }
		if stream.direction == 2 && bare_stream_flush(mut stream) < 0 { return 0 }
		if !bare_stream_buffer(mut stream) { return 0 }
		stream.direction = 1
		mut done := uint(0)
		total := size * count
		for done < total {
			if stream.pushed_count != 0 {
				stream.pushed_count--
				unsafe { (&u8(output))[done] = stream.pushed[stream.pushed_count] }
				done++
				continue
			}
			mut pending := stream.read_end - stream.read_at
			if pending != 0 {
				if pending > total - done { pending = total - done }
				unsafe { vmemcpy(voidptr(output + done), stream.buffer + stream.read_at, int(pending)) }
				stream.read_at += pending
				done += pending
				continue
			}
			if stream.abi_flags & i32(C.V_STDIO_EOF) != 0 { break }
			direct := stream.mode == i32(C._IONBF) || total - done >= stream.capacity
			buffer := if direct { output + done } else { uint(stream.buffer) }
			length := if direct { total - done } else { stream.capacity }
			read := bare_stream_raw_read(mut stream, buffer, length)
			if read < 0 {
				bare_stream_fail(mut stream, i32(-read))
				break
			}
			if read == 0 {
				stream.abi_flags |= i32(C.V_STDIO_EOF)
				break
			}
			if direct {
				done += uint(read)
			} else {
				stream.read_at = 0
				stream.read_end = uint(read)
			}
		}
		return done / size
	}
	@[export: 'v_stdio_write'; markused]
	fn bare_stdio_write(input uint, size uint, count uint, address uint) uint {
		mut stream := bare_stream(address)
		if size == 0 || count == 0 { return 0 }
		if !stream.writable {
			bare_stream_fail(mut stream, i32(C.EBADF))
			return 0
		}
		if count > uint(max_int) / size {
			bare_stream_fail(mut stream, i32(C.EOVERFLOW))
			return 0
		}
		if stream.orientation == 0 { stream.orientation = -1 }
		if stream.direction == 1 && bare_stream_flush(mut stream) < 0 { return 0 }
		if !bare_stream_buffer(mut stream) { return 0 }
		stream.direction = 2
		total := size * count
		data := unsafe { &u8(input) }
		mut line_end := uint(0)
		if stream.mode == i32(C._IOLBF) {
			for i in 0 .. int(total) { if unsafe { data[i] } == `\n` { line_end = uint(i + 1) } }
		}
		if stream.mode != i32(C._IONBF) && total <= stream.capacity - stream.write_at && line_end == 0 {
			unsafe { vmemcpy(stream.buffer + stream.write_at, data, int(total)) }
			stream.write_at += total
			return count
		}
		if bare_stream_flush(mut stream) < 0 { return 0 }
		mut direct := total
		if stream.mode == i32(C._IOLBF) {
			direct = if total - line_end <= stream.capacity { line_end } else { total }
		}
		mut done := uint(0)
		for done < direct {
			written := bare_stream_raw_write(mut stream, input + done, direct - done)
			if written <= 0 {
				bare_stream_fail(mut stream, if written < 0 { i32(-written) } else { i32(C.EIO) })
				return done / size
			}
			done += uint(written)
		}
		if done != total {
			unsafe { vmemcpy(stream.buffer, data + done, int(total - done)) }
			stream.write_at = total - done
		}
		return count
	}

	@[export: 'v_stdio_seek'; markused]
	fn bare_stdio_seek(address uint, offset_value i64, whence i32) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if stream.direction == 2 && bare_stream_flush(mut stream) < 0 { return -1 }
		mut offset := offset_value
		if whence == 1 && stream.direction == 1 {
			unread := stream.read_end - stream.read_at + stream.pushed_count
			if offset < (-i64(0x7fffffffffffffff) - 1) + i64(unread) {
				bf_native_errno_cell = i32(C.EOVERFLOW)
				return -1
			}
			offset -= i64(unread)
		}
		if bare_stream_seek(mut stream, offset, whence) < 0 { return -1 }
		stream.read_at = 0
		stream.read_end = 0
		stream.write_at = 0
		stream.pushed_count = 0
		stream.direction = 0
		stream.abi_flags &= ~i32(C.V_STDIO_EOF)
		stream.read_state = [8]u8{}
		stream.write_state = [8]u8{}
		return 0
	}
	@[export: 'v_stdio_tell'; markused]
	fn bare_stdio_tell(address uint) i64 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		mut position := bare_stream_seek(mut stream, 0, if stream.append && stream.write_at != 0 {
			i32(2)
		} else {
			i32(1)
		})
		if position < 0 { return -1 }
		adjustment := if stream.direction == 1 {
			-i64(stream.read_end - stream.read_at + stream.pushed_count)
		} else {
			i64(stream.write_at)
		}
		if adjustment > 0 && position > i64(0x7fffffffffffffff) - adjustment {
			bf_native_errno_cell = i32(C.EOVERFLOW)
			return -1
		}
		position += adjustment
		if position < 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		return position
	}
	@[export: 'v_stdio_state'; markused]
	fn bare_stdio_state(address uint, operation i32) i32 {
		mut stream := bare_stream(address)
		return match operation {
			0 { i32(stream.abi_flags & i32(C.V_STDIO_EOF) != 0) }
			1 { i32(stream.abi_flags & i32(C.V_STDIO_ERROR) != 0) }
			2 {
				stream.abi_flags &= ~i32(C.V_STDIO_EOF)
				stream.abi_flags &= ~i32(C.V_STDIO_ERROR)
				i32(0)
			}
			3 {
				if stream.fd < 0 { bf_native_errno_cell = i32(C.EBADF) }
				stream.fd
			}
			else { i32(-1) }
		}
	}
	@[export: 'v_stdio_setbuffer'; markused]
	fn bare_stdio_setbuffer(address uint, buffer uint, mode i32, requested uint) i32 {
		if mode != i32(C._IONBF) && mode != i32(C._IOLBF) && mode != i32(C._IOFBF) {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		if mode != i32(C._IONBF) && buffer != 0 && requested == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		size := if requested == 0 { uint(C.BUFSIZ) } else { requested }
		mut replacement := if mode == i32(C._IONBF) { uint(0) } else { buffer }
		if mode != i32(C._IONBF) && replacement == 0 {
			replacement = uint(unsafe { __malloc(size) })
			if replacement == 0 { return -1 }
		}
		bare_stdio_lock(address)
		mut stream := unsafe { &BareStream(address) }
		if bare_stream_flush(mut stream) < 0 {
			bare_stdio_unlock(address)
			if buffer == 0 { unsafe { __free(voidptr(replacement)) } }
			return -1
		}
		if stream.owns_buffer { unsafe { __free(stream.buffer) } }
		stream.buffer = unsafe { &u8(replacement) }
		stream.capacity = if mode == i32(C._IONBF) { uint(0) } else { size }
		stream.owns_buffer = replacement != 0 && buffer == 0
		stream.mode = mode
		stream.read_at = 0
		stream.read_end = 0
		stream.write_at = 0
		stream.pushed_count = 0
		bare_stdio_unlock(address)
		return 0
	}
	@[export: 'v_stdio_getchar'; markused]
	fn bare_stdio_getchar(address uint) i32 {
		mut stream := bare_stream(address)
		if !stream.readable { return bare_stream_fail(mut stream, i32(C.EBADF)) }
		if stream.direction == 1 {
			if stream.pushed_count != 0 {
				stream.pushed_count--
				return i32(stream.pushed[stream.pushed_count])
			}
			if stream.read_at < stream.read_end {
				value := unsafe { stream.buffer[stream.read_at] }
				stream.read_at++
				return i32(value)
			}
		}
		mut byte := u8(0)
		return if bare_stdio_read(uint(&byte), 1, 1, address) == 1 { i32(byte) } else { i32(-1) }
	}
	@[export: 'v_stdio_putchar'; markused]
	fn bare_stdio_putchar(value i32, address uint) i32 {
		byte := u8(value)
		return if bare_stdio_write(uint(&byte), 1, 1, address) == 1 { i32(byte) } else { i32(-1) }
	}
	@[export: 'v_stdio_fgets'; markused]
	fn bare_stdio_fgets(output uint, capacity i32, address uint) uint {
		if capacity <= 0 { return 0 }
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut used := 0
		for used < int(capacity) - 1 {
			value := bare_stdio_getchar(address)
			if value < 0 {
				if bare_stream(address).abi_flags & i32(C.V_STDIO_EOF) == 0 {
					// Like glibc, deliver bytes already read on a nonblocking stream.
					if used == 0 || bf_native_errno_cell != i32(C.EAGAIN) { return 0 }
				}
				break
			}
			unsafe { (&u8(output))[used] = u8(value) }
			used++
			if value == 10 { break }
		}
		if used == 0 && capacity > 1 { return 0 }
		unsafe { (&u8(output))[used] = 0 }
		return output
	}
	@[export: 'v_stdio_getdelim'; markused]
	fn bare_stdio_getdelim(line uint, capacity uint, delimiter i32, address uint) i64 {
		if line == 0 || capacity == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		mut used := uint(0)
		for {
			buffer := unsafe { *(&uint(line)) }
			size := unsafe { *(&uint(capacity)) }
			if buffer == 0 || used + 1 >= size {
				mut next := if buffer == 0 || size == 0 { uint(128) } else { size }
				if next <= used + 1 {
					if next > uint(max_int) / 2 {
						bare_stream_fail(mut stream, i32(C.ENOMEM))
						used = 0
						break
					}
					next *= 2
				}
				replacement := unsafe { realloc(voidptr(buffer), next) }
				if replacement == unsafe { nil } {
					bare_stream_fail(mut stream, i32(C.ENOMEM))
					used = 0
					break
				}
				unsafe {
					*(&uint(line)) = uint(replacement)
					*(&uint(capacity)) = next
				}
			}
			value := bare_stdio_getchar(address)
			if value < 0 {
				if stream.abi_flags & i32(C.V_STDIO_ERROR) != 0 { used = 0 }
				break
			}
			unsafe { (&u8(*(&uint(line))))[used] = u8(value) }
			used++
			if u8(value) == u8(delimiter) { break }
		}
		if unsafe { *(&uint(line)) } != 0 && unsafe { *(&uint(capacity)) } != 0 {
			unsafe { (&u8(*(&uint(line))))[used] = 0 }
		}
		return if used == 0 { i64(-1) } else { i64(used) }
	}
	@[export: 'v_stdio_ungetchar'; markused]
	fn bare_stdio_ungetchar(value i32, address uint) i32 {
		if value == -1 { return -1 }
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if !stream.readable || stream.pushed_count == 16 || (stream.direction == 2 && bare_stream_flush(mut stream) < 0) {
			return -1
		}
		stream.direction = 1
		stream.pushed[stream.pushed_count] = u8(value)
		stream.pushed_count++
		stream.abi_flags &= ~i32(C.V_STDIO_EOF)
		return i32(u8(value))
	}
	@[export: 'v_stdio_fread'; markused]
	fn bare_stdio_fread(output uint, size uint, count uint, address uint) uint {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		return bare_stdio_read(output, size, count, address)
	}
	@[export: 'v_stdio_fwrite'; markused]
	fn bare_stdio_fwrite(input uint, size uint, count uint, address uint) uint {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		return bare_stdio_write(input, size, count, address)
	}
	@[export: 'v_stdio_fgetc'; markused]
	fn bare_stdio_fgetc(address uint) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		return bare_stdio_getchar(address)
	}
	@[export: 'v_stdio_fputc'; markused]
	fn bare_stdio_fputc(value i32, address uint) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		return bare_stdio_putchar(value, address)
	}
	@[export: 'v_stdio_state_locked'; markused]
	fn bare_stdio_state_locked(address uint, operation i32) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		return bare_stdio_state(address, operation)
	}
	@[export: 'v_stdio_fputs'; markused]
	fn bare_stdio_fputs(text uint, address uint) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if stream.orientation > 0 { return -1 }
		stream.orientation = -1
		length := int(bare_string_chr_nul(text, 0) - text)
		return if bare_stdio_write(text, 1, length, address) == length { i32(0) } else { i32(-1) }
	}
	@[export: 'v_stdio_puts'; markused]
	fn bare_stdio_puts(text uint) i32 {
		address := C.v_stdio_stdout()
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		if bare_stdio_fputs(text, address) < 0 { return -1 }
		return if bare_stdio_putchar(10, address) >= 0 { i32(0) } else { i32(-1) }
	}
	@[export: 'v_stdio_setbuf'; markused]
	fn bare_stdio_setbuf(address uint, buffer uint) {
		bare_stdio_setbuffer(address, buffer, if buffer == 0 {
			i32(C._IONBF)
		} else {
			i32(C._IOFBF)
		}, uint(C.BUFSIZ))
	}
	@[export: 'v_stdio_rewind'; markused]
	fn bare_stdio_rewind(address uint) {
		bare_stdio_lock(address)
		bare_stdio_seek(address, 0, 0)
		bare_stdio_state(address, 2)
		bare_stdio_unlock(address)
	}
	@[export: 'v_stdio_tmpfile'; markused]
	fn bare_stdio_tmpfile() uint {
		mut fd := bare_native_openat(-100, uint(c'/tmp'), i32(C.O_TMPFILE) | i32(C.O_RDWR) | i32(C.O_CLOEXEC), 0o600)
		if fd < 0 {
			mut path := [20]u8{}
			pattern := '/tmp/vlibc-XXXXXX'
			for i in 0 .. pattern.len { path[i] = pattern[i] }
			fd = C.mkostemp(unsafe { &char(&path[0]) }, i32(C.O_CLOEXEC))
			if fd < 0 { return 0 }
			if bare_native_unlinkat(-100, uint(&path[0]), 0) < 0 {
				error := bf_native_errno_cell
				C.close(fd)
				bf_native_errno_cell = error
				return 0
			}
		}
		result := bare_stdio_fdopen(fd, uint(c'w+'))
		if result == 0 {
			error := bf_native_errno_cell
			C.close(fd)
			bf_native_errno_cell = error
		}
		return result
	}

	@[export: 'v_stdio_getpos'; markused]
	fn bare_stdio_getpos(address uint, output uint) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		position := bare_stdio_tell(address)
		if position < 0 { return -1 }
		stream := unsafe { &BareStream(address) }
		C.v_stdio_position_set(output, position, uint(&stream.read_state[0]))
		return 0
	}
	@[export: 'v_stdio_setpos'; markused]
	fn bare_stdio_setpos(address uint, input uint) i32 {
		mut state := [8]u8{}
		position := C.v_stdio_position_get(input, uint(&state[0]))
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		result := bare_stdio_seek(address, position, 0)
		if result == 0 {
			mut stream := unsafe { &BareStream(address) }
			stream.read_state = state
		}
		return result
	}
	fn bare_stream_orient_wide(mut stream BareStream) bool {
		if stream.orientation == 0 {
			stream.utf8 = i32(bare_wide_utf8(0))
			stream.orientation = 1
		}
		return stream.orientation > 0
	}
	@[export: 'v_stdio_orientation'; markused]
	fn bare_stdio_orientation(address uint, mode i32) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if mode > 0 {
			bare_stream_orient_wide(mut stream)
		} else if stream.orientation == 0 && mode < 0 {
			stream.orientation = -1
		}
		return stream.orientation
	}
	@[export: 'v_stdio_getwide'; markused]
	fn bare_stdio_getwide(address uint) u32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if !bare_stream_orient_wide(mut stream) { return 0xffffffff }
		mut value := u32(0)
		mut incomplete := false
		for {
			byte := bare_stdio_getchar(address)
			if byte < 0 {
				if incomplete { bare_stream_fail(mut stream, i32(C.EILSEQ)) }
				return 0xffffffff
			}
			input := u8(byte)
			count := C.v_stdio_decode(uint(&value), uint(&input), uint(&stream.read_state[0]), stream.utf8)
			if count == ~uint(0) {
				bare_stream_fail(mut stream, i32(C.EILSEQ))
				return 0xffffffff
			}
			if count == ~uint(1) {
				incomplete = true
				continue
			}
			return value
		}
		return 0xffffffff
	}
	@[export: 'v_stdio_putwide'; markused]
	fn bare_stdio_putwide(value u32, address uint) u32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if !bare_stream_orient_wide(mut stream) { return 0xffffffff }
		mut bytes := [16]u8{}
		mut next := stream.write_state
		size := C.v_stdio_encode(uint(&bytes[0]), value, uint(&next[0]), stream.utf8)
		if size == ~uint(0) {
			bare_stream_fail(mut stream, i32(C.EILSEQ))
			return 0xffffffff
		}
		if bare_stdio_write(uint(&bytes[0]), 1, size, address) != size { return 0xffffffff }
		stream.write_state = next
		return value
	}
	@[export: 'v_stdio_ungetwide'; markused]
	fn bare_stdio_ungetwide(value u32, address uint) u32 {
		if value == 0xffffffff { return 0xffffffff }
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if !bare_stream_orient_wide(mut stream) { return 0xffffffff }
		mut bytes := [16]u8{}
		mut state := [8]u8{}
		size := C.v_stdio_encode(uint(&bytes[0]), value, uint(&state[0]), stream.utf8)
		if size == ~uint(0) { return 0xffffffff }
		if !stream.readable || size > uint(stream.pushed.len) - stream.pushed_count || (stream.direction == 2 && bare_stream_flush(mut stream) < 0) {
			return 0xffffffff
		}
		mut i := size
		for i > 0 {
			i--
			stream.pushed[stream.pushed_count] = bytes[i]
			stream.pushed_count++
		}
		stream.read_state = [8]u8{}
		stream.direction = 1
		stream.abi_flags &= ~i32(C.V_STDIO_EOF)
		return value
	}
	@[export: 'v_stdio_getwides'; markused]
	fn bare_stdio_getwides(output uint, capacity i32, address uint) uint {
		if capacity <= 0 { return 0 }
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		old_error := stream.abi_flags & i32(C.V_STDIO_ERROR)
		stream.abi_flags &= ~i32(C.V_STDIO_ERROR)
		defer { stream.abi_flags |= old_error }
		mut used := 0
		for used < int(capacity) - 1 {
			value := bare_stdio_getwide(address)
			if value == 0xffffffff {
				if stream.abi_flags & i32(C.V_STDIO_ERROR) != 0 && bf_native_errno_cell != i32(C.EAGAIN) {
					return 0
				}
				break
			}
			unsafe { (&u32(output))[used] = value }
			used++
			if value == 10 { break }
		}
		if used == 0 && capacity > 1 { return 0 }
		unsafe { (&u32(output))[used] = 0 }
		return output
	}
	@[export: 'v_stdio_putwides'; markused]
	fn bare_stdio_putwides(input uint, address uint) i32 {
		bare_stdio_lock(address)
		defer { bare_stdio_unlock(address) }
		mut stream := unsafe { &BareStream(address) }
		if !bare_stream_orient_wide(mut stream) { return -1 }
		values := unsafe { &u32(input) }
		mut position := 0
		for unsafe { values[position] } != 0 {
			if bare_stdio_putwide(unsafe { values[position] }, address) == 0xffffffff { return -1 }
			position++
		}
		return 0
	}
}
