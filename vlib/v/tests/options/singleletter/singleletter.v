module singleletter

pub struct M {
pub:
	x int
}

// result returns a concrete one-letter struct in a result.
pub fn result(fail bool) !M {
	if fail {
		return error('failed')
	}
	return M{ x: 7 }
}

// optional returns a concrete one-letter struct in an option.
pub fn optional(present bool) ?M {
	if !present {
		return none
	}
	return M{ x: 8 }
}
