module alias_sumtype_is_module

pub type AliasIsScopeValue = bool | f64

// is_int checks the specialized value's int tag.
pub fn is_int[T](value T) bool {
	return value is int
}

// is_not_int checks that the specialized value does not hold an int.
pub fn is_not_int[T](value T) bool {
	return value !is int
}

// kind returns the name of the specialized variant.
pub fn kind[T](value T) string {
	return match value {
		int { 'int' }
		string { 'string' }
	}
}
