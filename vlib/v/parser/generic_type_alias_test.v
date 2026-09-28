module parser

import os
import v.pref

fn test_generic_type_alias_declarations_preserve_parameters() {
	path := os.join_path(os.temp_dir(), 'generic_type_alias_${os.getpid()}.v')
	os.write_file(path, 'module aliases

type Pointer[T] = &T

pub type MiddlewareHandler[T] = fn (mut T) bool

type Mapper[A, B] = fn (A) B

type Count = int
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	aliases := a.nodes.filter(it.kind == .type_decl)
	assert aliases.len == 4

	assert aliases[0].value == 'Pointer'
	assert aliases[0].generic_params() == ['T']
	assert aliases[0].typ == '&T'

	assert aliases[1].value == 'MiddlewareHandler'
	assert aliases[1].generic_params() == ['T']
	assert aliases[1].op == .arrow
	assert aliases[1].typ.starts_with('fn')
	assert aliases[1].typ.ends_with('bool')

	assert aliases[2].value == 'Mapper'
	assert aliases[2].generic_params() == ['A', 'B']

	// A following non-generic alias must not inherit the previous parameters.
	assert aliases[3].value == 'Count'
	assert aliases[3].generic_params().len == 0
	assert aliases[3].typ == 'int'
}
