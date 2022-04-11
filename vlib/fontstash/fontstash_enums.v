module fontstash

[flag]
pub enum Flags {
	top_left
	bottom_left
}

[flag]
pub enum Align {
	// Horizontal align
	left // Default
	center
	right
	// Vertical align
	top
	middle
	bottom
	baseline // Default
}

pub enum ErrorCode {
	// Font atlas is full.
	atlas_full = 1
	// Scratch memory used to render glyphs is full, requested size reported in 'val', you may need to bump up FONS_SCRATCH_BUF_SIZE.
	scratch_full = 2
	// Calls to fonsPushState has created too large stack, if you need deep state stack bump up FONS_MAX_STATES.
	states_overflow = 3
	// Trying to pop too many states fonsPopState().
	states_underflow = 4
}
