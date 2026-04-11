// Ownership extensions for strings.
// Only included when -ownership flag is passed (via -d ownership).
// Provides .to_owned() which creates an owned string with move semantics.
module builtin

// IClone marks values that provide explicit clone semantics in ownership mode.
//
// This is a marker interface. Concrete types still define their own `clone()`
// methods with concrete return types.
pub interface IClone {}

// to_owned creates an owned copy of the string.
// When ownership checking is enabled, owned strings have move semantics:
// assigning an owned string to another variable moves ownership,
// making the original variable invalid.
pub fn (s string) to_owned() string {
	return s.clone()
}
