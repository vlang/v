pub const omega = 3 // should be first

pub const alpha = 5 // should be in the middle

pub const beta = 2 // should be at the end

// def - should be first
pub fn def() {
	println(1)
}

// xyz - should be in the middle
// a small script <script>console.log('hello');</script>
// bold text <b>bold</b> end
// underlined text <u>underline</u> end
// a link [main v repo](https://github.com/vlang/v)
pub fn xyz() {
	println(2)
}

// abc - should be last
pub fn abc() {
	println(3)
}

// XampleDocComment is here just to group example doc comment tests.
pub struct XampleDocComment {}

// MyXMLDocument is here just to test the different combinations of methods/output types
pub struct MyXMLDocument {
	path string
}

// MyXMLDocument.from_text processes the file path, and returns an error
pub fn MyXMLDocument.from_file(path string) !MyXMLDocument {
	return error('TODO')
}

// MyXMLDocument.from_text processes text and produces none
pub fn MyXMLDocument.from_text(text string) ?MyXMLDocument {
	return none
}

// MyXMLDocument.abc does something too... I just do not know what.
pub fn MyXMLDocument.abc(text string) ?(string, int) {
	return 'xyz', 123
}

// instance_from_file does stuff with path
pub fn (x &MyXMLDocument) instance_from_file(path string) !MyXMLDocument {
	return error('TODO')
}

// instance_from_text does stuff with text
pub fn (x &MyXMLDocument) instance_from_text(text string) ?MyXMLDocument {
	return none
}

// instance_abc does stuff too
pub fn (x &MyXMLDocument) instance_abc(text string) ?(string, int) {
	return 'xyz', 123
}

// instance_void does stuff too
pub fn (x &MyXMLDocument) instance_void() {
	return 123
}

// instance_int does stuff too
pub fn (x &MyXMLDocument) instance_int() int {
	return 123
}

// instance_error does stuff too
pub fn (x &MyXMLDocument) instance_result() ! {
	return 123
}

// instance_option does stuff too
pub fn (x &MyXMLDocument) instance_option() ? {
	return 123
}

// Example: single()
pub fn (x XampleDocComment) single() {}

// single_multi concatanates an array with an arbitrary number of additional values
// NOTE: if you have two arrays, you should simply use the `<<` operator directly
// Example: single_multi([1, 2, 3], 4, 5, 6) == [1, 2, 3, 4, 5, 6] // => true
// Example: single_multi([1, 2, 3], ...[4, 5, 6]) == [1, 2, 3, 4, 5, 6] // => true
// Example: arr << [4, 5, 6] // does what you need if the arr is mutable
pub fn (x XampleDocComment) single_multi[T](a []T, b ...T) []T {
	return [1, 2, 3]
}

// multi rotates the array in-place
// Example:
// ```v
// mut x := [1,2,3,4,5,6]
// x.multi(2)
// println(x) // [5, 6, 1, 2, 3, 4]
// ```
pub fn (mut x XampleDocComment) multi[T](k int) {
}
