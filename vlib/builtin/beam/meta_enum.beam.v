module builtin

// EnumData holds information about an enum value
// Used by compile-time reflection ($for val in Enum.values)
pub struct EnumData {
pub:
	name  string
	value i64
	attrs []string
}
