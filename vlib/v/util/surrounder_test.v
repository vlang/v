module util

import strings

fn test_creation() {
	mut sr0 := new_surrounder(0)
	assert sr0.befores.cap == 0
	assert sr0.afters.cap == 0
	//
	mut sr10 := new_surrounder(10)
	assert sr10.befores.cap == 10
	assert sr10.afters.cap == 10
}

fn test_before_and_after() {
	mut sr := new_surrounder(0)
	sr.add('string tmp1;', 'string_free(&tmp1);')
	sr.add('string tmp2;', 'string_free(&tmp2);')
	start := sr.before()
	finish := sr.after()
	assert start == 'string tmp1;\nstring tmp2;\n'
	assert finish == 'string_free(&tmp2);\nstring_free(&tmp1);\n'
}

fn test_string_builder() {
	mut sr := new_surrounder(0)
	sr.add('x1', 'free x1')
	sr.add('x2', 'free x2')
	sr.add('x3', 'free x3')
	mut sb := strings.new_builder(512)
	sr.builder_write_befores(mut sb)
	sb.writeln('middle')
	sr.builder_write_afters(mut sb)
	s := sb.str()
	assert s == 'x1\nx2\nx3\nmiddle\nfree x3\nfree x2\nfree x1\n'
}
