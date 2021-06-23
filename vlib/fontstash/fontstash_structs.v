module fontstash

pub enum FonsFlags {
	top_left = 1
	bottom_left = 2
}

pub enum FonsAlign {
	// Horizontal align
	left 		= 1	// Default
	center 		= 2
	right 		= 4
	// Vertical align
	top 		= 8
	middle		= 16
	bottom		= 32
	baseline	= 64 // Default
}

pub enum FonsErrorCode {
	// Font atlas is full.
	atlas_full = 1
	// Scratch memory used to render glyphs is full, requested size reported in 'val', you may need to bump up FONS_SCRATCH_BUF_SIZE.
	scratch_full = 2
	// Calls to fonsPushState has created too large stack, if you need deep state stack bump up FONS_MAX_STATES.
	states_overflow = 3
	// Trying to pop too many states fonsPopState().
	states_underflow = 4
}

pub struct C.FONSparams {
	width int
	height int
	flags char
	userPtr voidptr
	// int (*renderCreate)(void* uptr, int width, int height)
	renderCreate fn(uptr voidptr, width int, height int) int
	// int (*renderResize)(void* uptr, int width, int height)
	renderResize fn(uptr voidptr, width int, height int) int
	// void (*renderUpdate)(void* uptr, int* rect, const unsigned char* data)
	renderUpdate fn(uptr voidptr, rect &int, data byteptr)
	// void (*renderDraw)(void* uptr, const float* verts, const float* tcoords, const unsigned int* colors, int nverts)
	renderDraw fn(uptr voidptr, verts &f32, tcoords &f32, colors &u32, nverts int)
	// void (*renderDelete)(void* uptr)
	renderDelete fn(uptr voidptr)
}

pub struct C.FONSquad
{
	x0 f32
	y0 f32
	s0 f32
	t0 f32
	x1 f32
	y1 f32
	s1 f32
	t1 f32
}

pub struct C.FONStextIter {
	x f32
	y f32
	nextx f32
	nexty f32
	scale f32
	spacing f32
	codepoint u32
	isize i16
	iblur i16
	font &C.FONSfont
	prevGlyphIndex int
	str byteptr
	next byteptr
	end byteptr
	utf8state u32
}

pub struct C.FONSfont {}

pub struct C.FONScontext {}
