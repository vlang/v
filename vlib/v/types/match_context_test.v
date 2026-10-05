module types

import os
import v.parser
import v.pref

fn test_match_diagnostics_preserve_borrowed_wrapped_contexts() {
	path := os.join_path(os.vtmp_dir(), 'match_context_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	source := 'module main

type Count = int

fn finished() ! {
}

fn choose_void(flag bool) ! {
 return match flag {
  true { finished() }
  false { error("missing") }
 }
}

fn choose_result(flag bool) !Count {
 return match flag {
  true { Count(42) }
  false { error("missing") }
 }
}

fn choose_option(flag bool) ?Count {
 return match flag {
  true { Count(42) }
  false { none }
 }
}

fn main() {
 choose_void(true)!
 assert choose_result(true)! == Count(42)
 assert choose_option(true)? == Count(42)
}
'
	os.write_file(path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([path])
	assert p.diagnostics.len == 0
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	for name in ['choose_void', 'choose_result'] {
		typ := tc.fn_ret_types[name] or { panic('missing ${name}') }
		assert typ is ResultType
	}
	typ := tc.fn_ret_types['choose_option'] or { panic('missing option') }
	assert typ is OptionType
}
