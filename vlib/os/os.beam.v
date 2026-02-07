// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module os

// BEAM backend stubs for os module
// These provide placeholder implementations that compile to valid BEAM code.
// Real implementations would use Erlang's file, filelib, and os modules.

// Command line arguments for BEAM backend
// Uses builtin.arguments() which is translated to init:get_plain_arguments() by codegen
@[markused]
pub const args = arguments()

// path_separator is the platform specific separator string
pub const path_separator = '/'

// path_delimiter is the platform specific delimiter string, used between paths in PATH
pub const path_delimiter = ':'

// path_devnull is a platform-specific file path of the null device
pub const path_devnull = '/dev/null'

// File permission constants
pub const s_ifmt = 0xF000
pub const s_ifdir = 0x4000
pub const s_ifreg = 0x8000
pub const s_iflnk = 0xa000
pub const s_isuid = 0o4000
pub const s_isgid = 0o2000
pub const s_isvtx = 0o1000
pub const s_irusr = 0o0400
pub const s_iwusr = 0o0200
pub const s_ixusr = 0o0100
pub const s_irgrp = 0o0040
pub const s_iwgrp = 0o0020
pub const s_ixgrp = 0o0010
pub const s_iroth = 0o0004
pub const s_iwoth = 0o0002
pub const s_ixoth = 0o0001

const executable_suffixes = ['']

const stdin_value = 0
const stdout_value = 1
const stderr_value = 2

// stdin returns an os.File for stdin.
pub fn stdin() File {
	return File{
		fd:        0
		cfile:     unsafe { nil }
		is_opened: true
	}
}

// stdout returns an os.File for stdout.
pub fn stdout() File {
	return File{
		fd:        1
		cfile:     unsafe { nil }
		is_opened: true
	}
}

// stderr returns an os.File for stderr.
pub fn stderr() File {
	return File{
		fd:        2
		cfile:     unsafe { nil }
		is_opened: true
	}
}

// PathKind identifies whether path is a file, directory, or link
struct PathKind {
mut:
	is_file bool
	is_dir  bool
	is_link bool
}

// CopyParams configures cp behavior
@[params]
pub struct CopyParams {
pub:
	fail_if_exists bool
}

// SystemError represents a system error
@[params]
pub struct SystemError {
pub:
	msg  string
	code int = -1
}

// DiskUsage contains disk space information
pub struct DiskUsage {
pub:
	total     u64
	available u64
	used      u64
}

// File struct for BEAM backend
pub struct File {
mut:
	cfile voidptr
pub:
	fd int
pub mut:
	is_opened bool
}

// SeekMode specifies the origin for seek operations
pub enum SeekMode {
	start
	current
	end
}

// close closes an opened file
pub fn (mut f File) close() {
	if !f.is_opened {
		return
	}
	f.is_opened = false
	// Real impl would use file:close/1
}

// write writes bytes to the file
pub fn (mut f File) write(buf []u8) !int {
	if !f.is_opened {
		return error_file_not_opened()
	}
	return buf.len
}

// writeln writes a string followed by newline to the file
pub fn (mut f File) writeln(s string) !int {
	if !f.is_opened {
		return error_file_not_opened()
	}
	// Placeholder - in real impl: file:write(F, S ++ "\n")
	return s.len + 1
}

// write_string writes a string to the file
pub fn (mut f File) write_string(s string) !int {
	if !f.is_opened {
		return error_file_not_opened()
	}
	// Placeholder - in real impl: file:write(F, S)
	return s.len
}

// write_ptr writes raw data to the file
pub fn (mut f File) write_ptr(data voidptr, size int) int {
	return size
}

// write_full_buffer writes all data from buffer to file
pub fn (mut f File) write_full_buffer(buffer voidptr, buffer_len usize) ! {
	// Placeholder
}

// flush flushes the file buffer
pub fn (mut f File) flush() {
	// Placeholder - in real impl: file:sync(F)
}

// seek moves the file cursor to a new location
pub fn (mut f File) seek(pos i64, mode SeekMode) ! {
	if !f.is_opened {
		return error_file_not_opened()
	}
	// Placeholder - in real impl: file:position(F, {bof/cur/eof, Offset})
}

// tell returns the current offset of the file cursor
pub fn (f &File) tell() !i64 {
	if !f.is_opened {
		return error_file_not_opened()
	}
	// Placeholder - in real impl: file:position(F, cur) -> {ok, Pos}
	return 0
}

// read reads bytes from file into buffer
pub fn (mut f File) read(mut buf []u8) !int {
	if !f.is_opened {
		return error_file_not_opened()
	}
	return 0
}

// read_bytes_with_newline reads bytes from file until newline or buffer is full
pub fn (f &File) read_bytes_with_newline(mut buf []u8) !int {
	if !f.is_opened {
		return error_file_not_opened()
	}
	if buf.len == 0 {
		return error(@FN + ': `buf.len` == 0')
	}
	// Stub for BEAM - would need io:get_line equivalent
	return 0
}

// read_into_ptr reads bytes from file into a raw pointer buffer
pub fn (mut f File) read_into_ptr(p &u8, max int) !int {
	if !f.is_opened {
		return error_file_not_opened()
	}
	// Stub for BEAM - would use file:read/2
	return 0
}

// eof returns true if the file cursor has reached the end of the file
pub fn (f &File) eof() bool {
	// Stub for BEAM - assume EOF
	return true
}

// Helper function for file not opened errors
fn error_file_not_opened() IError {
	return error('file not opened')
}

// ========================================
// Working directory functions
// ========================================

// getwd returns the absolute path of the current working directory.
// Real implementation would use file:get_cwd()
pub fn getwd() string {
	// Placeholder - in real impl: file:get_cwd() -> {ok, Dir}
	return '/tmp'
}

// chdir changes the current working directory to the new directory in `path`.
// Real implementation would use file:set_cwd/1
pub fn chdir(path string) ! {
	// Placeholder - in real impl: file:set_cwd(Path)
	if path == '' {
		return error('empty path')
	}
}

// ========================================
// File existence and type checks
// ========================================

// exists returns true if `path` (file or directory) exists.
// Real implementation would use filelib:is_file/1 or filelib:is_dir/1
pub fn exists(path string) bool {
	// Placeholder - in real impl: filelib:is_file(Path) orelse filelib:is_dir(Path)
	return false
}

// is_dir returns a `bool` indicating whether the given `path` is a directory.
// Real implementation would use filelib:is_dir/1
pub fn is_dir(path string) bool {
	// Placeholder - in real impl: filelib:is_dir(Path)
	return false
}

// Note: is_file is defined in os.v and uses exists() and is_dir()

// is_link returns a boolean indicating whether `path` is a link.
// Real implementation would check file_info
pub fn is_link(path string) bool {
	// Placeholder - in real impl: check element(3, file:read_link_info(Path)) == symlink
	return false
}

// is_executable returns `true` if `path` is executable.
pub fn is_executable(path string) bool {
	// Placeholder
	return false
}

// is_writable returns `true` if `path` is writable.
pub fn is_writable(path string) bool {
	// Placeholder
	return false
}

// is_readable returns `true` if `path` is readable.
pub fn is_readable(path string) bool {
	// Placeholder
	return false
}

// ========================================
// Directory operations
// ========================================

// mkdir creates a new directory with the specified path.
// Real implementation would use file:make_dir/1
pub fn mkdir(path string, params MkdirParams) ! {
	if path == '.' {
		return
	}
	// Placeholder - in real impl: file:make_dir(Path)
}

// rmdir removes a specified directory.
// Real implementation would use file:del_dir/1
pub fn rmdir(path string) ! {
	// Placeholder - in real impl: file:del_dir(Path)
}

// ls returns a list of files and dirs in the given `path`.
// Real implementation would use file:list_dir/1
pub fn ls(path string) ![]string {
	if path == '' {
		return error('ls() expects a folder, not an empty string')
	}
	// Placeholder - in real impl: file:list_dir(Path)
	return []string{}
}

// ========================================
// File operations
// ========================================

// read_file reads the file in `path` and returns the contents.
// Real implementation would use file:read_file/1
pub fn read_file(path string) !string {
	// Placeholder - in real impl: file:read_file(Path)
	return error('read_file not implemented for BEAM')
}

// read_bytes returns all bytes read from file in `path`.
pub fn read_bytes(path string) ![]u8 {
	content := read_file(path)!
	return content.bytes()
}

// Note: write_file is defined in os.v and uses create() and write_full_buffer()

// write_bytes writes all the `bytes` to `path`.
pub fn write_bytes(path string, bytes []u8) ! {
	// Placeholder
}

// rm removes file in `path`.
// Real implementation would use file:delete/1
pub fn rm(path string) ! {
	// Placeholder - in real impl: file:delete(Path)
}

// cp copies the file src to the file or directory dst.
// Real implementation would use file:copy/2
pub fn cp(src string, dst string, config CopyParams) ! {
	// Placeholder - in real impl: file:copy(Src, Dst)
}

// rename renames the file or folder from `src` to `dst`.
// Real implementation would use file:rename/2
pub fn rename(src string, dst string) ! {
	// Placeholder - in real impl: file:rename(Old, New)
}

// rename_dir renames the folder from `src` to `dst`.
pub fn rename_dir(src string, dst string) ! {
	rename(src, dst)!
}

// open tries to open a file from a given path for reading.
pub fn open(path string) !File {
	return error('open not implemented for BEAM')
}

// create creates or opens a file at a specified location.
pub fn create(path string) !File {
	return error('create not implemented for BEAM')
}

// vfopen returns an opened file, given its path and open mode.
// Note: For BEAM backend, this returns voidptr since C.FILE doesn't exist.
pub fn vfopen(path string, mode string) !voidptr {
	return error('vfopen not implemented for BEAM')
}

// ========================================
// File info and stat
// ========================================

// stat returns metadata about the given file/folder path.
// Real implementation would use file:read_file_info/1
pub fn stat(path string) !Stat {
	return error('stat not implemented for BEAM')
}

// lstat is similar to stat/1 for normal files/folders.
// Unlike stat/1, however, it will return the stat info for a symlink.
pub fn lstat(path string) !Stat {
	return error('lstat not implemented for BEAM')
}

// file_size returns the size in bytes, of the file located in `path`.
pub fn file_size(path string) u64 {
	return 0
}

// file_last_mod_unix returns the "last modified" time stamp of file.
pub fn file_last_mod_unix(path string) i64 {
	return 0
}

// truncate changes the size of the file located in `path` to `len`.
pub fn truncate(path string, len u64) ! {
	// Placeholder
}

// ========================================
// Environment variables
// ========================================

// getenv returns the value of the environment variable named by the key.
// Real implementation would use os:getenv/1
pub fn getenv(key string) string {
	return getenv_opt(key) or { '' }
}

// getenv_opt returns the value of a given environment variable.
// Returns `none` if the environment variable does not exist.
pub fn getenv_opt(key string) ?string {
	// Placeholder - in real impl: os:getenv(Key)
	return none
}

// setenv sets the value of an environment variable with `name` to `value`.
// Real implementation would use os:putenv/2
pub fn setenv(name string, value string, overwrite bool) int {
	// Placeholder - in real impl: os:putenv(Name, Value)
	return 0
}

// unsetenv clears an environment variable with `name`.
// Real implementation would use os:unsetenv/1
pub fn unsetenv(name string) int {
	// Placeholder - in real impl: os:unsetenv(Name)
	return 0
}

// environ returns a map of all the current environment variables.
// Real implementation would use os:getenv/0
pub fn environ() map[string]string {
	// Placeholder - in real impl: os:getenv() returns list of "KEY=VALUE" strings
	return map[string]string{}
}

// ========================================
// Path operations
// ========================================

// real_path returns the full absolute path for fpath, with all relative ../../, symlinks resolved.
// On BEAM: resolves '..' and '.' components but cannot resolve symlinks without Erlang stdlib.
pub fn real_path(fpath string) string {
	if fpath == '' {
		return getwd()
	}
	// If path is relative, prepend cwd
	mut p := fpath
	if p.len > 0 && p[0] != `/` {
		p = getwd() + '/' + p
	}
	// Resolve . and .. components
	parts := p.split('/')
	mut resolved := []string{}
	for part in parts {
		if part == '' || part == '.' {
			continue
		} else if part == '..' {
			if resolved.len > 0 {
				resolved.delete_last()
			}
		} else {
			resolved << part
		}
	}
	return '/' + resolved.join('/')
}

// Note: abs_path is defined in filepath.v and uses getwd() and join_path_single()

// symlink creates a symbolic link.
pub fn symlink(target string, link_name string) ! {
	// Placeholder - in real impl: file:make_symlink(Target, Link)
}

// readlink reads the target of a symbolic link.
pub fn readlink(path string) !string {
	// Placeholder - in real impl: file:read_link(Path)
	return error('readlink not implemented for BEAM')
}

// link creates a new link (hard link) to an existing file.
pub fn link(origin string, target string) ! {
	// Placeholder - in real impl: file:make_link(Origin, Target)
}

// ========================================
// System operations
// ========================================

// executable returns the path name of the executable that started the current process.
pub fn executable() string {
	// Placeholder - in real impl: could use escript:script_name() or init:get_argument(progname)
	return ''
}

// getpid returns the process ID (PID) of the calling process.
pub fn getpid() int {
	// Note: BEAM doesn't have traditional PIDs. Would need to convert Erlang PID.
	return 0
}

// getppid returns the process ID of the parent of the calling process.
pub fn getppid() int {
	return 0
}

// getuid returns the real user ID of the calling process.
pub fn getuid() int {
	return 0
}

// geteuid returns the effective user ID of the calling process.
pub fn geteuid() int {
	return 0
}

// getgid returns the real group ID of the calling process.
pub fn getgid() int {
	return 0
}

// getegid returns the effective group ID of the calling process.
pub fn getegid() int {
	return 0
}

// system executes the command and returns the exit code.
// Real implementation would use os:cmd/1 but that returns output, not exit code
pub fn system(cmd string) int {
	// Placeholder
	return 0
}

// execute starts the specified command, waits for it to complete, and returns its output.
// Real implementation would use os:cmd/1
pub fn execute(cmd string) Result {
	// Placeholder - in real impl: os:cmd(Cmd) returns output string
	return Result{
		exit_code: 0
		output:    ''
	}
}

// fork will fork the current system process.
pub fn fork() int {
	// BEAM doesn't support fork
	return -1
}

// wait blocks the calling process until one of its child processes exits.
pub fn wait() int {
	return -1
}

// ========================================
// User and system info
// ========================================

// Note: home_dir is defined in os.v and uses getenv()

// hostname returns the hostname.
// Falls back to HOSTNAME environment variable on BEAM.
pub fn hostname() !string {
	h := getenv('HOSTNAME')
	if h != '' {
		return h
	}
	// Try COMPUTERNAME (Windows-style, sometimes set)
	cn := getenv('COMPUTERNAME')
	if cn != '' {
		return cn
	}
	return 'localhost'
}

// loginname returns the name of the user logged in.
pub fn loginname() !string {
	return getenv('USER')
}

// uname returns information about the platform.
// On BEAM: returns BEAM VM information since we're running on the Erlang VM.
pub fn uname() Uname {
	h := hostname() or { 'localhost' }
	return Uname{
		sysname:  'BEAM'
		nodename: h
		release:  'OTP'
		version:  'Erlang/OTP'
		machine:  'beam_vm'
	}
}

// Note: user_os is defined in os.v and uses $if compile-time checks

// Note: temp_dir is defined in os.v and uses getenv()

// ========================================
// I/O operations
// ========================================

// flush will flush the stdout buffer.
pub fn flush() {
	// Placeholder
}

// get_raw_line returns a one-line string from stdin along with `\n`.
pub fn get_raw_line() string {
	// Placeholder - in real impl: io:get_line('')
	return ''
}

// get_raw_stdin will get the raw input from stdin.
pub fn get_raw_stdin() []u8 {
	return []u8{}
}

// fileno returns the file descriptor of an opened file.
pub fn fileno(cfile voidptr) int {
	return -1
}

// is_atty returns 1 if the `fd` file descriptor is open and refers to a terminal.
pub fn is_atty(fd int) int {
	return 0
}

// chmod changes file access attributes.
pub fn chmod(path string, mode int) ! {
	// Placeholder - in real impl: file:change_mode(Path, Mode)
}

// chown changes the owner and group attributes of `path`.
pub fn chown(path string, owner int, group int) ! {
	// Placeholder - in real impl: file:change_owner(Path, Owner, Group)
}

// ========================================
// Additional helper functions
// ========================================

// glob searches for all pathnames matching patterns.
// On BEAM: Basic implementation using ls() and pattern matching.
// For full glob support, Erlang's filelib:wildcard/1 would be needed via codegen.
pub fn glob(patterns ...string) ![]string {
	mut result := []string{}
	for pattern in patterns {
		mut matches := []string{}
		native_glob_pattern(pattern, mut matches)!
		for m in matches {
			result << m
		}
	}
	return result
}

// posix_get_error_msg returns error code representation in string.
// Maps common POSIX error codes to their descriptions.
pub fn posix_get_error_msg(code int) string {
	match code {
		0 { return 'success' }
		1 { return 'operation not permitted' }
		2 { return 'no such file or directory' }
		3 { return 'no such process' }
		4 { return 'interrupted system call' }
		5 { return 'input/output error' }
		9 { return 'bad file descriptor' }
		12 { return 'out of memory' }
		13 { return 'permission denied' }
		17 { return 'file exists' }
		20 { return 'not a directory' }
		21 { return 'is a directory' }
		22 { return 'invalid argument' }
		28 { return 'no space left on device' }
		36 { return 'file name too long' }
		else { return 'error code: ${code}' }
	}
}

// get_error_msg returns error code representation in string.
pub fn get_error_msg(code int) string {
	return posix_get_error_msg(code)
}

// last_error returns a V error formed by the last error.
pub fn last_error() IError {
	return error('unknown error')
}

// error_posix returns a POSIX error.
pub fn error_posix(e SystemError) IError {
	message := if e.msg == '' { 'posix error' } else { e.msg }
	return error_with_code(message, e.code)
}

// page_size returns the page size in bytes.
pub fn page_size() int {
	return 4096
}

// disk_usage returns disk usage of `path`.
pub fn disk_usage(path string) !DiskUsage {
	return DiskUsage{
		total:     0
		available: 0
		used:      0
	}
}

// native_glob_pattern is used for glob pattern matching.
// On BEAM: Lists directory entries and filters by simple pattern.
// Supports '*' wildcard at start/end but not full glob syntax.
fn native_glob_pattern(pattern string, mut matches []string) ! {
	// Extract directory and file pattern
	mut dir := '.'
	mut file_pattern := pattern
	// Find the last path separator
	mut last_sep := -1
	for i in 0 .. pattern.len {
		if pattern[i] == `/` {
			last_sep = i
		}
	}
	if last_sep >= 0 {
		dir = pattern[..last_sep]
		if dir == '' {
			dir = '/'
		}
		file_pattern = pattern[last_sep + 1..]
	}
	// List directory entries
	entries := ls(dir) or { return }
	for entry in entries {
		if match_glob_pattern(file_pattern, entry) {
			if dir == '.' {
				matches << entry
			} else {
				matches << dir + '/' + entry
			}
		}
	}
}

// match_glob_pattern performs simple glob matching.
// Supports: '*' matches any sequence, '?' matches single char.
fn match_glob_pattern(pattern string, name string) bool {
	if pattern == '*' {
		return true
	}
	if pattern == name {
		return true
	}
	// Simple prefix/suffix matching with *
	if pattern.len > 0 && pattern[0] == `*` {
		suffix := pattern[1..]
		if name.len >= suffix.len {
			return name[name.len - suffix.len..] == suffix
		}
		return false
	}
	if pattern.len > 0 && pattern[pattern.len - 1] == `*` {
		prefix := pattern[..pattern.len - 1]
		if name.len >= prefix.len {
			return name[..prefix.len] == prefix
		}
		return false
	}
	return false
}

// kind_of_existing_path identifies whether path is a file, directory, or link.
// Delegates to codegen-handled exists() and is_dir() functions.
fn kind_of_existing_path(path string) PathKind {
	if !exists(path) && !is_dir(path) {
		return PathKind{}
	}
	d := is_dir(path)
	return PathKind{
		is_file: !d
		is_dir:  d
		is_link: false // is_link requires Erlang stdlib call not available from pure V
	}
}

// input_password prompts the user for a password without echoing input.
// On BEAM: Would use io:get_password/1 or similar
pub fn input_password(prompt string) !string {
	// Placeholder - in real impl: would disable echo and read line
	return error('input_password not implemented for BEAM')
}

// execve replaces the current process with a new process.
// On BEAM: This concept doesn't directly apply to BEAM VM
pub fn execve(cmdpath string, cmdargs []string, envs []string) ! {
	// BEAM doesn't support exec-style process replacement
	return error('execve not implemented for BEAM')
}

// open_file opens a file with the given mode and options.
// Real implementation would use file:open/2
pub fn open_file(path string, mode string, options ...int) !File {
	// Placeholder - in real impl: file:open(Path, Modes)
	return error('open_file not implemented for BEAM')
}

// open_append opens a file for appending.
// Real implementation would use file:open(Path, [append, write])
pub fn open_append(path string) !File {
	// Placeholder - in real impl: file:open(Path, [append, write])
	return File{
		fd:        -1
		cfile:     unsafe { nil }
		is_opened: true
	}
}

// reopen reopens a file with the specified path and mode.
// Real implementation would close the current file and open the new one.
pub fn (mut f File) reopen(path string, mode string) ! {
	// Placeholder - in real impl: close and re-open
	f.close()
	// Re-open the file
	f.is_opened = true
}

// ========================================
// Signal handling
// ========================================

// signal_opt assigns a handler callback to be called when a signal is received.
// On BEAM: This is a stub - BEAM uses Erlang's process signal handling
// which is fundamentally different from POSIX signals.
// Returns the previous handler (always nil for BEAM stub).
pub fn signal_opt(signum Signal, handler SignalHandler) !SignalHandler {
	// BEAM doesn't support traditional POSIX signal handlers
	// Erlang processes use process links and monitors for similar functionality
	// Return a dummy handler to indicate "no previous handler"
	return handler
}

// signal_ignore masks system signals.
// On BEAM: This is a no-op since BEAM doesn't have traditional signal handling.
pub fn signal_ignore(args ...Signal) {
	// No-op on BEAM - Erlang handles this differently via process flags
}

// is_main_thread returns whether the current thread is the main thread.
// On BEAM: Always returns true since the concept doesn't directly apply.
fn is_main_thread() bool {
	return true
}

// signal_ignore_internal is the internal implementation for background threads.
// On BEAM: This is a no-op.
fn signal_ignore_internal(args ...Signal) {
	// No-op on BEAM
}

// write_file_array writes an array to a file.
pub fn write_file_array(path string, buffer array) ! {
	mut f := create(path)!
	unsafe {
		f.write_ptr(buffer.data, buffer.len * buffer.element_size)
	}
	f.close()
}
