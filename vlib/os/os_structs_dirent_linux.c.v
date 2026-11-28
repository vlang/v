module os

// The directory entry of Linux, with its fields in the order in which glibc, musl and bionic
// lay them out on a 64-bit system. A backend that takes the layout of a C struct from its V
// declaration, as the native backend does, reads `d_type` and `d_name` where this puts them.
pub struct C.dirent {
	d_ino    u64
	d_off    i64
	d_reclen u16
	d_type   u8
	d_name   [256]char
}
