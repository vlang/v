module fontstash

@[typedef]
pub struct C.FONSparams {
	width   i32
	height  i32
	flags   char
	userPtr voidptr
	// int (*renderCreate)(void* uptr, int width, int height)
	renderCreate fn (uptr voidptr, width i32, height i32) i32 = unsafe { nil }
	// int (*renderResize)(void* uptr, int width, int height)
	renderResize fn (uptr voidptr, width i32, height i32) i32 = unsafe { nil }
	// void (*renderUpdate)(void* uptr, int* rect, const unsigned char* data)
	renderUpdate fn (uptr voidptr, rect &i32, data &u8) = unsafe { nil }
	// void (*renderDraw)(void* uptr, const float* verts, const float* tcoords, const unsigned int* colors, int nverts)
	renderDraw fn (uptr voidptr, verts &f32, tcoords &f32, colors &u32, nverts i32) = unsafe { nil }
	// void (*renderDelete)(void* uptr)
	renderDelete fn (uptr voidptr) = unsafe { nil }
}

@[typedef]
pub struct C.FONSquad {
	x0 f32
	y0 f32
	s0 f32
	t0 f32
	x1 f32
	y1 f32
	s1 f32
	t1 f32
}

@[typedef]
pub struct C.FONStextIter {
	x              f32
	y              f32
	nextx          f32
	nexty          f32
	scale          f32
	spacing        f32
	codepoint      u32
	isize          i16
	iblur          i16
	font           &C.FONSfont = unsafe { nil }
	prevGlyphIndex i32
	str            &u8
	next           &u8
	end            &u8
	utf8state      u32
}

@[typedef]
pub struct C.FONSfont {}

@[typedef]
struct C.FONSstate {
	font    i32
	align   i32
	size    f32
	color   u32
	blur    f32
	spacing f32
}

@[typedef]
struct C.FONSatlas {}

@[typedef]
pub struct C.FONScontext {
pub:
	params  C.FONSparams
	itw     f32
	ith     f32
	texData &u8 = unsafe { nil }
mut:
	dirtyRect   [4]i32
	fonts       &&C.FONSfont = unsafe { nil }
	atlas       &C.FONSatlas = unsafe { nil }
	cfonts      i32
	nfonts      i32
	verts       [2048]f32
	tcoords     [2048]f32
	colors      [1024]u32
	nverts      i32
	scratch     &u8 = unsafe { nil }
	nscratch    i32
	states      [20]C.FONSstate
	nstates     i32
	handleError fn (voidptr, i32, i32) = unsafe { nil }
	errorUptr   voidptr
}
