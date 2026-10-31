module strconv

// parse_bool converts these boolean literals:
// "1", "t", "T", "TRUE", "true", "True" -> true and
// "0", "f", "F", "FALSE", "false", "False" -> false.
// Anything else is an error (unlike string.bool(), which defaults to false).
// It is the V-idiomatic equivalent of Go's strconv.ParseBool.
pub fn parse_bool(s string) !bool {
	match s {
		'1', 't', 'T', 'true', 'TRUE', 'True' { return true }
		'0', 'f', 'F', 'false', 'FALSE', 'False' { return false }
		else { return error('strconv: ParseBool: parsing "${s}": invalid syntax') }
	}
}
