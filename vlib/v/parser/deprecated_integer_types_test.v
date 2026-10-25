module parser

import v.flat
import v.pref

fn test_deprecated_integer_type_spellings_warn_at_each_type_token() {
	sources := {
		'fn identity[T](x T) T { return x }\nstruct Box[T] { value T }\nfn main() { _ = identity[Box[usize]](Box[usize]{}) }': 2
		'type Old = isize':                                                                                                    1
		'struct S { values []map[isize]usize }':                                                                               2
		'fn main() { _ = usize(1) }':                                                                                          1
		'fn take(x isize) usize { return usize(x) }':                                                                          3
		'fn take(x fn (isize) usize) {}':                                                                                      2
		'fn main() { _ = sizeof(isize) }':                                                                                     1
		'fn main() { _ = [1]usize[1] }':                                                                                       1
		'fn identity[T](x T) T { return x }
fn main() { _ = identity[isize](0) }':                                             1
	}
	for source, count in sources {
		mut p := Parser.new(pref.new_preferences())
		p.parse_text('deprecated.v', source)
		assert p.diagnostics.len == count, '${source}\n${p.diagnostics}'
		for diagnostic in p.diagnostics {
			assert diagnostic.severity == 'warning:', diagnostic.str()
			old := source[diagnostic.pos.offset..diagnostic.pos.end]
			assert old in ['isize', 'usize'], old
			replacement := if old == 'isize' { 'int' } else { 'uint' }
			assert diagnostic.message == '`${old}` is deprecated; use `${replacement}` instead'
		}
	}
}

fn test_deprecated_integer_types_compile_as_platform_integer_types() {
	mut p := Parser.new(pref.new_preferences())
	a := p.parse_text('aliases.v', 'type Signed = isize\ntype Unsigned = usize\nfn main() { _ = usize(1) }')
	aliases := a.nodes.filter(it.kind == .type_decl)
	assert aliases.len == 2
	assert aliases[0].typ == flat.source_type_isize
	assert aliases[1].typ == flat.source_type_uint
	casts := a.nodes.filter(it.kind == .cast_expr)
	assert casts.len == 1
	assert casts[0].value == 'uint'
}

fn test_modern_integer_types_and_ordinary_names_do_not_warn() {
	mut p := Parser.new(pref.new_preferences())
	p.parse_text('modern.v', "struct Names { isize int; usize uint }
fn main() {
	isize := int(-1)
	usize := uint(1)
	_ = Names{isize: isize, usize: usize}
	_ = 'isize usize'
	// isize usize
}")
	assert p.diagnostics.len == 0, p.diagnostics.str()
}

fn test_formatter_preserves_deprecated_integer_spellings_without_warnings() {
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	mut p := Parser.new(prefs)
	a := p.parse_text('legacy.v', 'type Signed = isize\nfn main() { _ = usize(1) }')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	casts := a.nodes.filter(it.kind == .cast_expr)
	assert casts.len == 1
	assert casts[0].value == 'usize'
}

fn test_deprecated_integer_warnings_do_not_exhaust_parser_error_limit() {
	mut p := Parser.new(pref.new_preferences())
	source := 'fn main() {\n' + '\t_ = usize(1)\n'.repeat(max_parse_diagnostics + 1) + '}\n'
	p.parse_text('many_deprecations.v', source)
	assert p.diagnostics.len == max_parse_diagnostics + 1
	assert p.diagnostics.all(it.severity == 'warning:')
	assert !p.diagnostic_limit_reached
	for _ in 0 .. max_parse_diagnostics + 1 {
		p.record_diagnostic('test error', 0)
	}
	assert p.diagnostic_limit_reached
	assert p.diagnostics.last().message == 'too many errors; stopping after ${max_parse_diagnostics} diagnostics'
}
