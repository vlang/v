// normal comment
pub fn a1() {
	println('hi')
}

// this should be merged
// into the same line
pub fn a2() {
	println('hi')
}

// This should be its own paragraph.
//
// This should be another paragraph.
pub fn a3() {
	println('hi')
}

// This should be merged
// into one paragraph.
// Note: this should be its own paragraph.
pub fn a4() {
	println('hi')
}

// This should be its own paragraph.
// NOTE: this should also be it own paragraph
// note: this should be its own paragraph.
pub fn a5() {
	println('hi')
}

// A comment
// Fixme: this should be its own paragraph.
// fixme: this should be its own paragraph.
// FIXME: this should be its own paragraph.
pub fn a6() {
	println('hi')
}

// A comment
// TODO: this should be its own paragraph.
// todo: this should be its own paragraph.
// Todo: this should be its own paragraph.
pub fn a7() {
	println('hi')
}

// A comment
// TODO: this should be its own paragraph.
// NOTE: this should be its own paragraph.
// FIXME: this should be its own paragraph.
pub fn a8() {
	println('hi')
}

// Some info that should not be added as doc comment
// A align      2 bit  Note: for now only 1 used!
// U uppercase  1 bit  0 do nothing, 1 do to_upper()

// normal comment
pub fn a9() {
	println('hi')
}

// foo does stuff
// ```
// this is a multiline codeblock.
// second line
// third line.
// ```
pub fn foo() {}
