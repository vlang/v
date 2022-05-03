pub const (
	source_root = 'temp' // some const
	another     = int(5) //
)

// Used to indicate that you don't care what the window position is.
pub const (
	windowpos_undefined_mask = C.SDL_WINDOWPOS_UNDEFINED_MASK //   0x1FFF0000u
	windowpos_undefined      = C.SDL_WINDOWPOS_UNDEFINED //
)

// funky - comment for function below
pub fn funky() {
	println('hi')
}
