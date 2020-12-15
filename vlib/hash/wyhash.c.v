module hash

//#flag -I @VROOT/thirdparty/wyhash
//#include "wyhash.h"
fn C.wyhash(byteptr, u64, u64) u64

[inline]
pub fn wyhash_c(key byteptr, len u64, seed u64) u64 {
	return C.wyhash(key, len, seed)
}

