module constevalhelper

pub struct SelectorCell {
pub:
	value int
}

// The sibling file imports a module as `evaluated`. A cached header combines
// that import with these bodies, but the local and parameter keep precedence.
@[comptime]
pub fn local_selector() int {
	evaluated := SelectorCell{ value: 7 }
	return evaluated.value
}

@[comptime]
pub fn parameter_selector(evaluated SelectorCell) int {
	return evaluated.value
}

@[comptime]
pub fn qualified_selector() int {
	return constevalhelper.seed
}
