module callbacks

pub type ErrorCallback = fn (path string, err IError) int

// invoke passes its path and error to the callback.
pub fn invoke(callback ErrorCallback) int {
	return callback('entry', error('unavailable'))
}

// invoke_inline accepts the same callback without a named function type.
pub fn invoke_inline(callback fn (string, IError) int) int {
	return callback('inline', error('missing'))
}
