module types

import os
import v.parser
import v.pref

const bodyless_method_call_msg = 'cannot call a method that does not have a body'

fn check_bodyless_method_source(name string, source string) TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'bodyless_method_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	return tc
}

fn test_bodyless_method_call_is_rejected_only_on_its_receiver() {
	tc := check_bodyless_method_source('receiver', 'type Foo = int\ntype Bar = int\nfn (_ Foo) f()\nfn (_ Bar) f() {}\nfn (_ Foo) g() {}\nfn main() {\n\tFoo(0).f()\n\tBar(0).f()\n\tFoo(0).g()\n}\n')
	assert 'f' in tc.source_no_body_method_names
	assert 'g' !in tc.source_no_body_method_names
	assert tc.errors.filter(it.msg == bodyless_method_call_msg).len == 1, tc.errors.str()
}

fn test_method_with_a_body_is_not_prescreened() {
	tc := check_bodyless_method_source('plain', 'type Foo = int\nfn (_ Foo) g() {}\nfn main() {\n\tFoo(0).g()\n}\n')
	assert 'g' !in tc.source_no_body_method_names
	assert !tc.errors.any(it.msg == bodyless_method_call_msg), tc.errors.str()
}
