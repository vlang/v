module json_sibling_hook

// This custom json2 encode hook for User lives in a sibling file of the same module.
// v fmt parses only the file it formats, so the input file's in-file scan cannot see it.
// The `@[manualfree]` attribute checks that the sibling-hook scan skips leading attributes
// rather than giving up on the declaration.
@[manualfree]
pub fn (u User) to_json() string {
	return '"custom"'
}
