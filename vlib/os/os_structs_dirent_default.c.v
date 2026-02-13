module os

pub struct C.dirent {
	d_ino     u64
	d_seekoff u64
	d_reclen  u16
	d_namlen  u16
	d_type    u8
	d_name    [256]char
}
