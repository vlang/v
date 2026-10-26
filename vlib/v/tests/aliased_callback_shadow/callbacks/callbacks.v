module callbacks

pub struct Response {
pub:
	value int
}

pub type Handler = fn (bool) !Response

// forward returns the aliased callback's result unchanged.
pub fn forward(h Handler, fail bool) !Response {
	return h(fail)
}

// dispatch calls a callback obtained through a map guard.
pub fn dispatch(handlers map[string]Handler, fail bool) !Response {
	if h := handlers['custom'] {
		return h(fail)
	}
	return error('missing handler')
}
