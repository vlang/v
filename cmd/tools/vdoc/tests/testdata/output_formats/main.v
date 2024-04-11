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
