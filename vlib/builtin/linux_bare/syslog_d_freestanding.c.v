@[has_globals]
module builtin

import time

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include <sys/socket.h>
	#include <syslog.h>
	#include "@DIR/libc_time_abi.h"
	fn C.v_time_local_offset(i64) i64
	#include <fcntl.h>
	fn C.v_freestanding_log_format(uint, uint, uint) i32
	fn C.socket(i32, i32, i32) i32
	fn C.v_freestanding_log_connect(i32, uint, u32) i32
	fn C.send(i32, voidptr, uint, i32) i64
	fn C.close(i32) i32
	fn C.open(&char, i32, ...voidptr) i32
	fn C.write(i32, voidptr, uint) i64
	fn C.getpid() i32

	struct BareLogAddress {
	mut:
		family u16
		path   [108]u8
	}

	struct BareLogState {
	mut:
		lock_word  u32
		fd         i32
		connected  bool
		stream     bool
		options    i32
		facility   i32
		mask       i32
		configured bool
		ident      &char = unsafe { nil }
	}

	__global bare_log BareLogState

	fn bare_log_lock() {
		for !bare_cas_u32(&bare_log.lock_word, 0, 1) {
			bf_futex_wait(&bare_log.lock_word, 1, -1)
		}
		if !bare_log.configured {
			bare_log.fd = -1
			bare_log.facility = i32(C.LOG_USER)
			bare_log.mask = 255
			bare_log.configured = true
		}
	}

	fn bare_log_unlock() {
		bare_store_u32(&bare_log.lock_word, 0)
		bf_futex_wake(&bare_log.lock_word, 1)
	}

	fn bare_log_disconnect() {
		if bare_log.connected { C.close(bare_log.fd) }
		bare_log.fd = -1
		bare_log.connected = false
	}

	fn bare_log_connect() bool {
		if bare_log.connected { return true }
		mut address := BareLogAddress{ family: u16(C.AF_UNIX) }
		path := '/dev/log'
		for i in 0 .. path.len { address.path[i] = path[i] }
		for stream in [false, true]! {
			kind := if stream { i32(C.SOCK_STREAM) } else { i32(C.SOCK_DGRAM) }
			fd := C.socket(i32(C.AF_UNIX), kind | i32(C.SOCK_CLOEXEC), 0)
			if fd < 0 { return false }
			mut result := i32(-1)
			for {
				result = C.v_freestanding_log_connect(fd, uint(&address), u32(sizeof(BareLogAddress)))
				if result >= 0 || bf_native_errno_cell != i32(Errno.eintr) { break }
			}
			if result == 0 {
				bare_log.fd = fd
				bare_log.connected = true
				bare_log.stream = stream
				return true
			}
			error := bf_native_errno_cell
			C.close(fd)
			if stream || error != 91 { return false } // EPROTOTYPE: try the stream logger.
		}
		return false
	}

	@[export: 'v_freestanding_openlog'; markused]
	fn bare_openlog(ident uint, options i32, facility i32) {
		saved_errno := bf_native_errno_cell
		bare_log_lock()
		bare_log.ident = unsafe { &char(ident) }
		bare_log.options = options
		if facility & ~i32(C.LOG_FACMASK) == 0 { bare_log.facility = facility }
		if options & i32(C.LOG_NDELAY) != 0 { bare_log_connect() }
		bare_log_unlock()
		bf_native_errno_cell = saved_errno
	}

	@[export: 'v_freestanding_closelog'; markused]
	fn bare_closelog() {
		saved_errno := bf_native_errno_cell
		bare_log_lock()
		bare_log_disconnect()
		bare_log.ident = unsafe { nil }
		bare_log_unlock()
		bf_native_errno_cell = saved_errno
	}

	@[export: 'v_freestanding_setlogmask'; markused]
	fn bare_setlogmask(mask i32) i32 {
		bare_log_lock()
		previous := bare_log.mask
		if mask != 0 { bare_log.mask = mask }
		bare_log_unlock()
		return previous
	}

	fn bare_log_write_line(fd i32, text &u8, length int, ending string) {
		mut position := 0
		for position < length {
			written := C.write(fd, unsafe { text + position }, uint(length - position))
			if written < 0 && bf_native_errno_cell == i32(Errno.eintr) { continue }
			if written <= 0 { return }
			position += int(written)
		}
		position = 0
		for position < ending.len {
			written := C.write(fd, unsafe { ending.str + position }, uint(ending.len - position))
			if written < 0 && bf_native_errno_cell == i32(Errno.eintr) { continue }
			if written <= 0 { return }
			position += int(written)
		}
	}

	fn bare_log_send(record string) bool {
		length := record.len + if bare_log.stream { 1 } else { 0 }
		mut position := 0
		for position < length {
			written := C.send(bare_log.fd, unsafe { record.str + position }, uint(length - position), i32(C.MSG_NOSIGNAL))
			if written < 0 && bf_native_errno_cell == i32(Errno.eintr) { continue }
			if written <= 0 { return false }
			position += int(written)
			if !bare_log.stream && position != length { return false }
		}
		return true
	}

	@[export: 'v_freestanding_syslog'; markused]
	fn bare_syslog(priority_value i32, format uint, arguments uint) {
		saved_errno := bf_native_errno_cell
		defer { bf_native_errno_cell = saved_errno }
		if priority_value & ~(i32(C.LOG_FACMASK) | i32(C.LOG_PRIMASK)) != 0 { return }
		bare_log_lock()
		defer { bare_log_unlock() }
		if bare_log.mask & (i32(1) << (priority_value & i32(C.LOG_PRIMASK))) == 0 { return }
		mut priority := priority_value
		if priority & i32(C.LOG_FACMASK) == 0 { priority |= bare_log.facility }
		mut body_ptr := unsafe { &char(nil) }
		bf_native_errno_cell = saved_errno
		length := C.v_freestanding_log_format(uint(&body_ptr), format, arguments)
		if length < 0 { return }
		defer { unsafe { __free(body_ptr) } }
		body := unsafe { tos(&u8(body_ptr), int(length)) }
		message := if body.len > 0 && body[body.len - 1] == `\n` {
			body[..body.len - 1]
		} else {
			body
		}
		epoch, _, _ := bf_realtime_now()
		calendar := time.unix(epoch + C.v_time_local_offset(epoch))
		mut stamp_bytes := [16]u8{}
		months := 'JanFebMarAprMayJunJulAugSepOctNovDec'
		for i in 0 .. 3 { stamp_bytes[i] = months[(calendar.month - 1) * 3 + i] }
		stamp_bytes[3] = ` `
		stamp_bytes[4] = if calendar.day < 10 { u8(` `) } else { u8(`0` + calendar.day / 10) }
		stamp_bytes[5] = u8(`0` + calendar.day % 10)
		stamp_bytes[6] = ` `
		stamp_bytes[9] = `:`
		stamp_bytes[12] = `:`
		for i, part in [calendar.hour, calendar.minute, calendar.second]! {
			stamp_bytes[7 + i * 3] = u8(`0` + part / 10)
			stamp_bytes[8 + i * 3] = u8(`0` + part % 10)
		}
		stamp := unsafe { tos(&stamp_bytes[0], 15) }
		ident := if bare_log.ident != unsafe { nil } {
			unsafe { tos2(&u8(bare_log.ident)) }
		} else {
			bare_program_short_name()
		}
		pid := if bare_log.options & i32(C.LOG_PID) != 0 { '[${C.getpid()}]' } else { '' }
		defer { if pid.len > 0 { unsafe { pid.free() } } }
		plain := '${ident}${pid}: ${message}'
		defer { unsafe { plain.free() } }
		record := '<${priority}>${stamp} ${plain}'
		defer { unsafe { record.free() } }
		if bare_log.options & i32(C.LOG_PERROR) != 0 {
			bare_log_write_line(2, plain.str, plain.len, '\n')
		}
		mut delivered := false
		for _ in 0 .. 2 {
			if !bare_log_connect() { break }
			if bare_log_send(record) {
				delivered = true
				break
			}
			bare_log_disconnect()
		}
		if !delivered && bare_log.options & i32(C.LOG_CONS) != 0 {
			fd := C.open(c'/dev/console', i32(C.O_WRONLY) | i32(C.O_NOCTTY) | i32(C.O_CLOEXEC))
			if fd >= 0 {
				bare_log_write_line(fd, plain.str, plain.len, '\r\n')
				C.close(fd)
			}
		}
	}
}
