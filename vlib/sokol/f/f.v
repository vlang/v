module f

import fontstash
import sokol.c

pub const used_import = fontstash.used_import + c.used_import

#flag linux -I.

//#include "ft2build.h"

#define SOKOL_FONTSTASH_IMPL
#include "util/sokol_fontstash.h"

// salloc - used in the allocator structs, that the SOKOL libraries use, for allocating new memory blocks
pub fn salloc(size usize, user_data voidptr) voidptr {
	return unsafe { malloc(int(size)) }
}

// sfree - used in the allocator structs, that the SOKOL libraries use, for freeing memory
fn sfree(ptr voidptr, user_data voidptr) {
	unsafe { free(ptr) }
}
