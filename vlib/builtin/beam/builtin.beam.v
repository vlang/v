// BEAM backend builtins
// These functions are translated by the BEAM codegen to runtime calls
module builtin

// println prints a line to stdout
// Codegen translates to: vbeam_io:println(...)
pub fn println(s string) {
	// BEAM codegen handles this
}

// print prints to stdout without newline
// Codegen translates to: vbeam_io:print(...)
pub fn print(s string) {
	// BEAM codegen handles this
}

// eprintln prints a line to stderr
// Codegen translates to: vbeam_io:eprintln(...)
pub fn eprintln(s string) {
	// BEAM codegen handles this
}

// eprint prints to stderr without newline
// Codegen translates to: vbeam_io:eprint(...)
pub fn eprint(s string) {
	// BEAM codegen handles this
}

// exit terminates the program
// Codegen translates to: erlang:halt(Code)
@[noreturn]
pub fn exit(code int) {
	// BEAM codegen handles this - translates to erlang:halt
	for {}
}

// panic crashes with a message
// Codegen translates to: vbeam_panic:panic(...)
@[noreturn]
pub fn panic(s string) {
	// BEAM codegen handles this
	for {}
}

// flush_stdout - noop on BEAM (stdout is unbuffered)
pub fn flush_stdout() {
}

// flush_stderr - noop on BEAM (stderr is unbuffered)
pub fn flush_stderr() {
}

// unbuffer_stdout will turn off the default buffering done for stdout.
// On BEAM: This is a no-op because BEAM's I/O is already message-based
// and doesn't use the same buffering as C's stdio.
pub fn unbuffer_stdout() {
	// No-op on BEAM - Erlang's I/O system handles this differently
}

// print_backtrace prints the current stack trace
pub fn print_backtrace() {
	// Codegen translates to: vbeam_panic:print_stacktrace()
}

// arguments returns command line arguments
// Codegen translates to: init:get_plain_arguments()
pub fn arguments() []string {
	// BEAM codegen handles this
	return []
}

// panic_n prints an error message, followed by the given number, then exits.
@[noreturn]
pub fn panic_n(s string, n i64) {
	panic(s)
	for {}
}

// panic_n2 prints an error message, followed by two numbers, then exits.
@[noreturn]
pub fn panic_n2(s string, number1 i64, number2 i64) {
	panic(s)
	for {}
}

// panic_n3 prints an error message, followed by three numbers, then exits.
@[noreturn]
fn panic_n3(s string, number1 i64, number2 i64, number3 i64) {
	panic(s)
	for {}
}

// vmemcpy copies n bytes from src to dest
// The memory areas should not overlap
@[unsafe]
pub fn vmemcpy(dest voidptr, const_src voidptr, n isize) voidptr {
	// BEAM codegen handles this - maps to binary operations
	return dest
}

// vmemmove copies n bytes from src to dest
// The memory areas can overlap
@[unsafe]
pub fn vmemmove(dest voidptr, const_src voidptr, n isize) voidptr {
	// BEAM codegen handles this - maps to binary operations
	return dest
}

// vmemset fills the first n bytes of memory area s with constant byte c
@[unsafe]
pub fn vmemset(s voidptr, c int, n isize) voidptr {
	// BEAM codegen handles this
	return s
}

// vcalloc dynamically allocates a zeroed n bytes block of memory
@[unsafe]
pub fn vcalloc(n isize) &u8 {
	// BEAM codegen handles this
	return unsafe { nil }
}

// isnil returns true if an object is nil
@[inline]
pub fn isnil(v voidptr) bool {
	return v == 0
}

// copy copies elements from src to dest
// Returns the number of elements copied (minimum of src.len and dest.len)
// On BEAM: This is a stub - actual copying is handled by codegen
// as Erlang lists don't have data pointers like C arrays
pub fn copy(mut dst []u8, src []u8) int {
	min := if dst.len < src.len { dst.len } else { src.len }
	// BEAM codegen handles the actual list manipulation
	return min
}

// vmemcmp compares two memory regions byte by byte
// Returns 0 if equal, <0 if s1 < s2, >0 if s1 > s2
// On BEAM: This would compare binaries
@[inline; unsafe]
pub fn vmemcmp(const_s1 voidptr, const_s2 voidptr, n isize) int {
	// BEAM codegen handles this - binary comparison
	return 0
}

// malloc allocates sz bytes of memory
// On BEAM: This is a stub - BEAM has automatic memory management
// Codegen translates to appropriate binary/list allocation
@[unsafe]
pub fn malloc(sz int) &u8 {
	return unsafe { nil }
}

// malloc_noscan allocates sz bytes of memory (no GC scan needed)
// On BEAM: Same as malloc - BEAM GC handles everything
@[unsafe]
pub fn malloc_noscan(sz int) &u8 {
	return unsafe { nil }
}

// free deallocates memory at ptr
// On BEAM: No-op - BEAM has automatic garbage collection
@[unsafe]
pub fn free(ptr voidptr) {
	// No-op on BEAM - garbage collected
}
