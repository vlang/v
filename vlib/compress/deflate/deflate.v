module deflate

#flag -I @VROOT/thirdparty/zip
#include "miniz.h"

// Ref - miniz.h
pub const (
	no_compression      = 0
	best_speed          = 1
	best_compression    = 9
	uber_compression    = 10
	default_level       = 6
	default_compression = -1
)

fn C.mz_compressBound(u64) u64
fn C.mz_error(err int) charptr
fn C.mz_compress2(charptr, &u32, charptr, u32, int) int
fn C.mz_uncompress(charptr, &u32, charptr, u32) int

pub fn compress(src []byte) ?[]byte {
	return compress2(src, deflate.default_compression)
}

pub fn compress2(src []byte, level int) ?[]byte {
	mut dest_len := C.mz_compressBound(src.len)
	mut dest := unsafe { malloc(int(dest_len)) }
	mz_res := C.mz_compress2(dest, &dest_len, src.data, src.len, level)

	if mz_res != 0 {
		return error(unsafe { C.mz_error(mz_res).vstring() })
	}

	return unsafe { dest.vbytes(int(dest_len)) }
}

pub fn uncompress(src []byte) ?[]byte {
	mut dest_len := int(src.len * 3)
	mut dest := unsafe { malloc(int(dest_len)) }

	mz_res := C.mz_uncompress(dest, &dest_len, src.data, src.len)

	if mz_res != 0 {
		return error(unsafe { C.mz_error(mz_res).vstring() })
	}

	return unsafe { dest.vbytes(int(dest_len)) }
}
