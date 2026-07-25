module builder

import v.ast

fn test_repro_short_name() {
	assert repro_short_name('main.Foo.bar') == 'bar'
	assert repro_short_name('math.abs') == 'abs'
	assert repro_short_name('Foo') == 'Foo'
}

fn test_is_user_repro_file() {
	assert is_user_repro_file('/home/me/project/main.v')
	assert !is_user_repro_file('/usr/local/lib/v/vlib/os/os.v')
	assert !is_user_repro_file('/home/me/.vmodules/pcre/pcre.v')
	assert !is_user_repro_file('/opt/v/thirdparty/tcc/x.v')
	assert !is_user_repro_file('/home/me/project/notes.txt')
	assert !is_user_repro_file('')
}

fn test_repro_identifiers_skips_strings_and_comments() {
	ids :=
		repro_identifiers("fn foo() {\n\t// call bar_in_comment here\n\tx := 'str word_in_str'\n\tbaz(x)\n}")
	assert 'foo' in ids
	assert 'baz' in ids
	assert 'x' in ids
	// words inside a comment or a plain string must not be treated as references
	assert 'bar_in_comment' !in ids
	assert 'word_in_str' !in ids
}

fn test_repro_identifiers_collects_string_interpolation() {
	ids := repro_identifiers("println('value is \${my_const} and \${helper()}')")
	assert 'my_const' in ids
	assert 'helper' in ids
}

fn test_repro_attr_start_walks_over_attributes() {
	lines := ['fn a() {}', '', '@[direct_array_access]', '@[inline]', 'fn b() {}']
	// `b` is on line 4 (0-based), its attributes start on line 2
	assert repro_attr_start(lines, 4) == 2
	// `a` has no attributes
	assert repro_attr_start(lines, 0) == 0
}

fn test_repro_decl_source_trims_trailing_blanks() {
	lines := ['fn a() {', '\tx := 1', '}', '', '', 'fn b() {}']
	assert repro_decl_source(lines, 0, 5) == 'fn a() {\n\tx := 1\n}'
	assert repro_decl_source(lines, 5, 6) == 'fn b() {}'
}

fn test_repro_closure_follows_references() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() { helper(Thing{}) }'
		},
		ReproDecl{
			names:  ['helper']
			source: 'fn helper(t Thing) {}'
		},
		ReproDecl{
			names:  ['Thing']
			source: 'struct Thing {}'
		},
		ReproDecl{
			names:  ['unrelated']
			source: 'fn unrelated() {}'
		},
	]
	name_to_decl := {
		'main':      [0]
		'helper':    [1]
		'Thing':     [2]
		'unrelated': [3]
	}
	order := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert order == [0, 1, 2]
	assert 3 !in order
}

fn test_repro_closure_seeds_from_main_for_reachability() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() { println(helper().len) }'
		},
		ReproDecl{
			names:  ['helper']
			source: 'fn helper() []Thing { return []Thing{} }'
		},
		ReproDecl{
			names:  ['Thing']
			source: 'struct Thing {}'
		},
	]
	name_to_decl := {
		'main':   [0]
		'helper': [1]
		'Thing':  [2]
	}
	order := repro_closure(decls, name_to_decl, [1, 0]) or {
		assert false, 'closure returned none'
		return
	}
	assert 0 in order
	assert 1 in order
	assert 2 in order
}

fn test_repro_closure_falls_back_when_cap_exceeded() {
	// a chain `main -> d0 -> d1 -> ...` longer than the declaration cap: every link is required,
	// so the closure must report incomplete (none) rather than return a truncated program
	n := c_error_repro_max_decls + 5
	mut decls := [ReproDecl{
		names:  ['main']
		source: 'fn main() { d0() }'
	}]
	mut name_to_decl := {
		'main': [0]
	}
	for i in 0 .. n {
		next := if i + 1 < n { 'd${i + 1}' } else { 'end' }
		decls << ReproDecl{
			names:  ['d${i}']
			source: 'fn d${i}() { ${next}() }'
		}
		name_to_decl['d${i}'] = [decls.len - 1]
	}
	mut hit_cap := true
	if _ := repro_closure(decls, name_to_decl, [0]) {
		hit_cap = false
	}
	assert hit_cap
}

fn test_repro_import_stmt_reconstructs_forms() {
	assert repro_import_stmt(ast.Import{ mod: 'os', source_name: 'os' }) == 'import os'
	assert repro_import_stmt(ast.Import{ mod: 'math.bits', source_name: 'math.bits' }) == 'import math.bits'
	assert repro_import_stmt(ast.Import{ mod: 'os', source_name: 'os', alias: 'o' }) == 'import os as o'
	assert repro_import_stmt(ast.Import{ mod: 'log', source_name: 'log', alias: '_' }) == 'import log as _'
	selective := repro_import_stmt(ast.Import{
		mod:         'math'
		source_name: 'math'
		syms:        [ast.ImportSymbol{ name: 'abs' }, ast.ImportSymbol{ name: 'max' }]
	})
	assert selective == 'import math { abs, max }'
	// combined alias + selective form must keep both, or a `t.month_days` reference breaks
	combined := repro_import_stmt(ast.Import{
		mod:         'time'
		source_name: 'time'
		alias:       't'
		syms:        [ast.ImportSymbol{ name: 'Time' }]
	})
	assert combined == 'import time as t { Time }'
}

fn test_repro_operator_refs() {
	plus := repro_operator_refs('return a + b')
	assert '+' in plus
	index := repro_operator_refs('return a[i]')
	assert '[]' in index
	assert '[]=' in index
	cmp := repro_operator_refs('return a < b')
	assert '<' in cmp
	eq := repro_operator_refs('return a == b')
	assert '==' in eq
	// an operator method is pulled into the closure when the operator is used
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() { _ := Vec{} + Vec{} }'
		},
		ReproDecl{
			names:  ['Vec']
			source: 'struct Vec {}'
		},
		ReproDecl{
			names:  ['+']
			source: 'fn (a Vec) + (b Vec) Vec { return a }'
		},
	]
	name_to_decl := {
		'main': [0]
		'Vec':  [1]
		'+':    [2]
	}
	order := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert 2 in order // the `+` operator method is included
}

fn test_repro_render_emits_module_and_referenced_imports() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() {\n\tprintln(os.args.len)\n}'
		},
	]
	imports := [
		ReproImport{
			source:   'import os'
			triggers: ['os']
		},
		ReproImport{
			source:   'import math'
			triggers: ['math']
		},
	]
	out := repro_render(imports, [], decls, [0])
	assert out.starts_with('module main')
	assert out.contains('import os')
	assert !out.contains('import math')
	assert out.contains('fn main() {')
}

fn test_repro_render_keeps_selective_and_side_effect_imports() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() { println(abs(-1)) }'
		},
	]
	imports := [
		ReproImport{
			source:   'import math { abs }'
			triggers: ['math', 'abs']
		},
		ReproImport{
			source:      'import log as _'
			triggers:    ['_']
			side_effect: true
		},
	]
	out := repro_render(imports, [], decls, [0])
	// the selective import is kept because `abs` (its selected symbol) is referenced
	assert out.contains('import math { abs }')
	// the side-effect import is always kept
	assert out.contains('import log as _')
}

fn test_repro_render_includes_hash_directives() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() { C.puts(c"hi".str) }'
		},
	]
	out := repro_render([], ['#include <stdio.h>'], decls, [0])
	assert out.contains('#include <stdio.h>')
}

fn test_repro_render_excludes_unreferenced_local_import() {
	// a local import that nothing references must not be emitted (and if it were referenced, the
	// builder path falls back entirely — see v_source_reproducer)
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() { println(1) }'
		},
	]
	imports := [
		ReproImport{
			source:   'import foo'
			triggers: ['foo']
			is_local: true
		},
	]
	out := repro_render(imports, [], decls, [0])
	assert !out.contains('import foo')
}
