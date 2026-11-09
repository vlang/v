// package: wasi:filesystem
// version: 0.2.8

// interface types
// use wasi:io/streams@0.2.8 { input-stream, output-stream, error }
// use wasi:clocks/wall-clock@0.2.8 { datetime }
pub type Filesize = u64
pub enum Descriptor_type {
	unknown
	block_device
	character_device
	directory
	fifo
	symbolic_link
	regular_file
	socket
}
pub type Descriptor_flags = int
pub const descriptor_flags_read = 1
pub const descriptor_flags_write = 2
pub const descriptor_flags_file_integrity_sync = 4
pub const descriptor_flags_data_integrity_sync = 8
pub const descriptor_flags_requested_write_sync = 16
pub const descriptor_flags_mutate_directory = 32
pub struct Descriptor_stat {
	type Descriptor_type
	link_count Link_count
	size Filesize
	data_access_timestamp ?voidptr
	data_modification_timestamp ?voidptr
	status_change_timestamp ?voidptr
}
pub type Path_flags = int
pub const path_flags_symlink_follow = 1
pub type Open_flags = int
pub const open_flags_create = 1
pub const open_flags_directory = 2
pub const open_flags_exclusive = 4
pub const open_flags_truncate = 8
pub type Link_count = u64
pub struct New_timestamp_No_change {}
pub struct New_timestamp_Now {}
pub struct New_timestamp_Timestamp {
	value voidptr
}
pub type New_timestamp = New_timestamp_No_change | New_timestamp_Now | New_timestamp_Timestamp
pub struct Directory_entry {
	type Descriptor_type
	name string
}
pub enum Error_code {
	access
	would_block
	already
	bad_descriptor
	busy
	deadlock
	quota
	exist
	file_too_large
	illegal_byte_sequence
	in_progress
	interrupted
	invalid
	io
	is_directory
	loop
	too_many_links
	message_size
	name_too_long
	no_device
	no_entry
	no_lock
	insufficient_memory
	insufficient_space
	not_directory
	not_empty
	not_recoverable
	unsupported
	no_tty
	no_such_device
	overflow
	not_permitted
	pipe
	read_only
	invalid_seek
	text_file_busy
	cross_device
}
pub enum Advice {
	normal
	sequential
	random
	will_need
	dont_need
	no_reuse
}
pub struct Metadata_hash_value {
	lower u64
	upper u64
}
pub struct Descriptor {}
pub fn (mut self Descriptor) read_via_stream(offset Filesize) !voidptr {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) write_via_stream(offset Filesize) !voidptr {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) append_via_stream() !voidptr {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) advise(offset Filesize, length Filesize, advice Advice) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) sync_data() ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) get_flags() !Descriptor_flags {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) get_type() !Descriptor_type {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) set_size(size Filesize) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) set_times(data_access_timestamp New_timestamp, data_modification_timestamp New_timestamp) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) read(length Filesize, offset Filesize) !([]u8, bool) {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) write(buffer []u8, offset Filesize) !Filesize {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) read_directory() !Directory_entry_stream {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) sync() ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) create_directory_at(path string) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) stat() !Descriptor_stat {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) stat_at(path_flags Path_flags, path string) !Descriptor_stat {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) set_times_at(path_flags Path_flags, path string, data_access_timestamp New_timestamp, data_modification_timestamp New_timestamp) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) link_at(old_path_flags Path_flags, old_path string, new_descriptor voidptr, new_path string) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) open_at(path_flags Path_flags, path string, open_flags Open_flags, flags Descriptor_flags) !Descriptor {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) readlink_at(path string) !string {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) remove_directory_at(path string) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) rename_at(old_path string, new_descriptor voidptr, new_path string) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) symlink_at(old_path string, new_path string) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) unlink_file_at(path string) ! {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) is_same_object(other voidptr) bool {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) metadata_hash() !Metadata_hash_value {
	panic("TODO: implement")
}
pub fn (mut self Descriptor) metadata_hash_at(path_flags Path_flags, path string) !Metadata_hash_value {
	panic("TODO: implement")
}
pub struct Directory_entry_stream {}
pub fn (mut self Directory_entry_stream) read_directory_entry() ?Directory_entry {
	panic("TODO: implement")
}