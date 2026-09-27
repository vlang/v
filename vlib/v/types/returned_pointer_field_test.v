module types

import os
import v.parser
import v.pref

fn test_returning_field_address_keeps_immutable_alias_checks() {
	path := os.join_path(os.vtmp_dir(), 'v3_field_address_alias_${os.getpid()}.v')
	os.write_file(path, 'struct Item { mut: value int }
struct Holder { item Item }
fn (h &Holder) address() &Item { return &h.item }
fn main() {
 h := Holder{item: Item{value: 1}}
 mut value := h.address()
 value.value = 2
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.any(it.msg == '`value.value` aliases mutable data from an immutable value'), tc.errors.str()
}
