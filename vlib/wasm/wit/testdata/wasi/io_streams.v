// package: wasi:io
// version: 0.2.8

// interface streams
// use error { error }
// use poll { pollable }
pub struct Stream_error_Last_operation_failed {
	value voidptr
}
pub struct Stream_error_Closed {}
pub type Stream_error = Stream_error_Last_operation_failed | Stream_error_Closed
pub struct Input_stream {}
pub fn (mut self Input_stream) read(len u64) ![]u8 {
	panic("TODO: implement")
}
pub fn (mut self Input_stream) blocking_read(len u64) ![]u8 {
	panic("TODO: implement")
}
pub fn (mut self Input_stream) skip(len u64) !u64 {
	panic("TODO: implement")
}
pub fn (mut self Input_stream) blocking_skip(len u64) !u64 {
	panic("TODO: implement")
}
pub fn (mut self Input_stream) subscribe() int {
	panic("TODO: implement")
}
pub struct Output_stream {}
pub fn (mut self Output_stream) check_write() !u64 {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) write(contents []u8) ! {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) blocking_write_and_flush(contents []u8) ! {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) flush() ! {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) blocking_flush() ! {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) subscribe() int {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) write_zeroes(len u64) ! {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) blocking_write_zeroes_and_flush(len u64) ! {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) splice(src voidptr, len u64) !u64 {
	panic("TODO: implement")
}
pub fn (mut self Output_stream) blocking_splice(src voidptr, len u64) !u64 {
	panic("TODO: implement")
}