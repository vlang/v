module main

import encoding.utf8.validate
import os
import strings

// These layouts match the Win32 structs. os.Process exits the caller when a
// Windows launch fails, so MCP uses checked Win32 calls without that wrapper.
struct CompilerSecurityAttributes {
	length     u32
	descriptor voidptr
	inherit    i32
}

struct CompilerStartupInfo {
	cb            u32
	reserved      &u16 = unsafe { nil }
	desktop       &u16 = unsafe { nil }
	title         &u16 = unsafe { nil }
	x             u32
	y             u32
	x_size        u32
	y_size        u32
	x_chars       u32
	y_chars       u32
	fill          u32
	flags         u32
	show          u16
	reserved_size u16
	reserved_data &u8 = unsafe { nil }
	stdin_handle  voidptr
	stdout_handle voidptr
	stderr_handle voidptr
}

struct CompilerProcessInfo {
mut:
	process voidptr
	thread  voidptr
	pid     u32
	tid     u32
}

// windows_compiler_launch_error captures GetLastError before cleanup changes it.
fn windows_compiler_launch_error(stage string) CompilerRun {
	err := os.last_error()
	return CompilerRun{
		exit_code:    err.code()
		launch_error: '${stage}: ${err.msg()}'
	}
}

// run_compiler_windows starts one compiler child with literal argv and cwd,
// merged output, and EOF stdin. Every launch failure is returned to the caller.
fn run_compiler_windows(executable string, args []string, root string) CompilerRun {
	mut input := voidptr(0)
	mut reader := voidptr(0)
	mut writer := voidptr(0)
	mut child := CompilerProcessInfo{}
	// Win32 represents an invalid file handle with the pointer value -1.
	invalid_handle := unsafe { voidptr(-1) }
	defer {
		for handle in [input, reader, writer, child.process, child.thread] {
			if handle != voidptr(0) && handle != invalid_handle {
				C.CloseHandle(handle)
			}
		}
	}
	security := CompilerSecurityAttributes{
		length:  sizeof(CompilerSecurityAttributes)
		inherit: 1
	}
	if !C.CreatePipe(&reader, &writer, voidptr(&security), 0) {
		return windows_compiler_launch_error('CreatePipe')
	}
	if !C.SetHandleInformation(reader, C.HANDLE_FLAG_INHERIT, 0) {
		return windows_compiler_launch_error('SetHandleInformation')
	}
	null_path := os.path_devnull.to_wide()
	defer { unsafe { free(null_path) } }
	input = C.CreateFileW(null_path, C.GENERIC_READ,
		C.FILE_SHARE_READ | C.FILE_SHARE_WRITE | C.FILE_SHARE_DELETE, voidptr(&security),
		C.OPEN_EXISTING, C.FILE_ATTRIBUTE_NORMAL, voidptr(0))
	if input == invalid_handle {
		return windows_compiler_launch_error('CreateFileW stdin')
	}
	startup := CompilerStartupInfo{
		cb:            sizeof(CompilerStartupInfo)
		flags:         u32(C.STARTF_USESTDHANDLES)
		stdin_handle:  input
		stdout_handle: writer
		stderr_handle: writer
	}
	application := executable.to_wide()
	command_line := windows_compiler_command_line(executable, args).to_wide()
	directory := root.to_wide()
	defer {
		unsafe {
			free(application)
			free(command_line)
			free(directory)
		}
	}
	if !C.CreateProcessW(application, command_line, voidptr(0), voidptr(0), true,
		C.CREATE_NO_WINDOW, voidptr(0), directory, voidptr(&startup), voidptr(&child)) {
		return windows_compiler_launch_error('CreateProcessW')
	}
	C.CloseHandle(input)
	input = voidptr(0)
	C.CloseHandle(writer)
	writer = voidptr(0)
	mut output := strings.new_builder(1024)
	defer { unsafe { output.free() } }
	mut buffer := [4096]u8{}
	mut read_count := u32(0)
	for {
		// ReadFile writes at most buffer.len bytes into the fixed buffer.
		read_ok := unsafe {
			C.ReadFile(reader, &buffer[0], u32(buffer.len), &read_count, voidptr(0))
		}
		if !read_ok || read_count == 0 {
			break
		}
		output.write_string(buffer[..int(read_count)].bytestr())
	}
	mut exit_code := u32(0)
	C.WaitForSingleObject(child.process, C.INFINITE)
	C.GetExitCodeProcess(child.process, &exit_code)
	return CompilerRun{
		exit_code: int(exit_code)
		output:    windows_compiler_output(output.str())
	}
}

// windows_compiler_output preserves UTF-8, recognizes UTF-16 output, and
// converts the Windows ANSI code page when the captured bytes are not UTF-8.
fn windows_compiler_output(raw string) string {
	if raw.len < 2 {
		return windows_compiler_text_output(raw)
	}
	mut little_endian := raw[0] == 0xff && raw[1] == 0xfe
	mut big_endian := raw[0] == 0xfe && raw[1] == 0xff
	sample_pairs := if raw.len > 128 { 64 } else { raw.len / 2 }
	mut high_zeros := 0
	mut low_zeros := 0
	for i in 0 .. sample_pairs {
		if raw[i * 2] == 0 { low_zeros++ }
		if raw[i * 2 + 1] == 0 { high_zeros++ }
	}
	little_endian = little_endian || high_zeros * 4 >= sample_pairs * 3
	big_endian = big_endian || low_zeros * 4 >= sample_pairs * 3
	if little_endian || big_endian {
		start := if raw[..2] in ['\xff\xfe', '\xfe\xff'] { 2 } else { 0 }
		pairs := (raw.len - start) / 2
		mut wide := []u16{len: pairs + 1}
		for i in 0 .. pairs {
			first := u16(raw[start + i * 2])
			second := u16(raw[start + i * 2 + 1])
			wide[i] = if little_endian { first | (second << 8) } else { (first << 8) | second }
		}
		// wide holds the decoded UTF-16 units for the duration of conversion.
		decoded := unsafe { string_from_wide2(wide.data, pairs) }
		unsafe { wide.free() }
		return decoded
	}
	return windows_compiler_text_output(raw)
}

// windows_compiler_text_output converts byte-oriented output when it uses the
// Windows ANSI code page rather than UTF-8, including one-byte strings.
fn windows_compiler_text_output(raw string) string {
	if validate.utf8_string(raw) {
		return raw
	}
	wide := raw.to_wide(from_ansi: true)
	defer { unsafe { free(wide) } }
	return unsafe { string_from_wide(wide) }
}
