module fontstash

pub enum FonsFlags {
	top_left = 1
	bottom_left = 2
}

pub enum FonsAlign {
	// Horizontal align
	left = 1 // Default
	center = 2
	right = 4
	// Vertical align
	top = 8
	middle = 16
	bottom = 32
	baseline = 64 // Default
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
