module sfons

import fontstash
import sokol.f

// keep v from warning about unused imports
const used_import = f.used_import + fontstash.used_import + 1

[inline]
pub fn create(width int, height int, flags int) &fontstash.Context {
	return C.sfons_create(width, height, flags)
}

[inline]
pub fn destroy(ctx &fontstash.Context) {
	C.sfons_destroy(ctx)
}

[inline]
pub fn rgba(r u8, g u8, b u8, a u8) u32 {
	return C.sfons_rgba(r, g, b, a)
}

[inline]
pub fn flush(ctx &fontstash.Context) {
	C.sfons_flush(ctx)
}
