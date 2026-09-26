module fastc

fn C.ftruncate(i32, u64) i32

fn C._chsize_s(i32, u64) i32

fn fastc_truncate_file_descriptor(fd i32, len u64) i32 {
	$if windows {
		return C._chsize_s(fd, len)
	} $else {
		return C.ftruncate(fd, len)
	}
}
