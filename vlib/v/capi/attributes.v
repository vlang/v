module capi

// CApiSkip excludes a public function from the generated C surface.
@[attribute]
pub struct CApiSkip {}

// CApiName overrides the generated C symbol name for a function.
@[attribute]
pub struct CApiName {
pub:
	name string
}

// CApiExport includes a private function in the generated C surface.
@[attribute]
pub struct CApiExport {}

// CApiErrno sets the C status returned when a `!T` function fails. Without it,
// the generated wrapper returns -1 and sets the thread-local error message.
@[attribute]
pub struct CApiErrno {
pub:
	code int
}
