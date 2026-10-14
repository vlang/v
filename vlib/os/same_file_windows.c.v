module os

// ByHandleFileInformation mirrors the C struct of the same name; field order
// and all-u32 layout are what the Windows ABI requires (FILETIME pairs are
// two DWORDs each).
struct ByHandleFileInformation {
mut:
	dw_file_attributes      u32
	ft_creation_lo          u32
	ft_creation_hi          u32
	ft_last_access_lo       u32
	ft_last_access_hi       u32
	ft_last_write_lo        u32
	ft_last_write_hi        u32
	dw_volume_serial_number u32
	n_file_size_high        u32
	n_file_size_low         u32
	n_number_of_links       u32
	n_file_index_high       u32
	n_file_index_low        u32
}

// FileIdentity is the stable object identity of a file on Windows.
struct FileIdentity {
	vol u32
	hi  u32
	lo  u32
}

fn C.CreateFileA(name &char, access u32, share u32, security voidptr, creation u32, flags u32, template voidptr) voidptr

fn C.GetFileInformationByHandle(h voidptr, info &ByHandleFileInformation) i32

fn C.CloseHandle(h voidptr) bool

// file_identity_windows returns the object identity of path.
fn file_identity_windows(path string) !FileIdentity {
	// access 0 = query metadata only; OPEN_EXISTING
	h := C.CreateFileA(&char(path.str), 0, 7, unsafe { nil }, 3, 0x80, unsafe { nil })
	if isnil(h) || h == voidptr(-1) {
		return error('os: cannot open `${path}`')
	}
	defer {
		C.CloseHandle(h)
	}
	mut info := ByHandleFileInformation{}
	if C.GetFileInformationByHandle(h, &info) == 0 {
		return error('os: cannot query identity of `${path}`')
	}
	return FileIdentity{info.dw_volume_serial_number, info.n_file_index_high, info.n_file_index_low}
}

// same_file_windows compares object identity through the file index.
fn same_file_windows(a string, b string) !bool {
	ia := file_identity_windows(a)!
	ib := file_identity_windows(b)!
	return ia == ib
}
