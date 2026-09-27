module builder

import v.ast
import v.token

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

fn test_repro_identifiers_skips_nested_literals_in_interpolation() {
	// a string/char literal nested inside a `${...}` interpolation must be skipped just like a
	// top-level literal, while a real reference in the same interpolation is still collected
	ids := repro_identifiers("println('\${pick(cond, 'word_in_nested_str', other_ref)}')")
	assert 'pick' in ids
	assert 'other_ref' in ids
	assert 'word_in_nested_str' !in ids
}

fn test_repro_identifiers_skips_rune_and_comment_in_interpolation() {
	// a `}` inside a backtick rune literal must not prematurely close the interpolation scan, so a
	// reference that follows it still enters the closure
	ids1 := repro_identifiers("_ := '\${ f(`}`) + helper() }'")
	assert 'f' in ids1
	assert 'helper' in ids1
	// a `}` inside a block comment inside the interpolation likewise
	ids2 := repro_identifiers("_ := '\${ a /* } */ + helper() }'")
	assert 'helper' in ids2
	// a `}` inside a line comment inside the interpolation likewise
	ids3 := repro_identifiers("_ := '\${ a // }\n + helper() }'")
	assert 'helper' in ids3
}

fn test_repro_uses_local_resource() {
	assert repro_uses_local_resource('const data = \$embed_file("logo.png")')
	assert repro_uses_local_resource('x := \$tmpl("page.html")')
	assert repro_uses_local_resource('s := \$res("a.txt")')
	// ordinary source, and unrelated comptime constructs, are not local resources
	assert !repro_uses_local_resource('fn main() { println(1) }')
	assert !repro_uses_local_resource('\$if linux { println("x") }')
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
	// The unrelated decl is excluded, so the closure is a strict subset: the
	// reproducer keeps it (order.len < decls.len).
	assert order.len < decls.len
}

fn test_repro_closure_reaching_every_decl_signals_whole_program() {
	// When the closure reaches every declaration, order.len == decls.len. The
	// reproducer treats that as whole-program coverage and gives up, so a short
	// all-referenced program is never uploaded whole (PR #28131 review).
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
	]
	name_to_decl := {
		'main':   [0]
		'helper': [1]
		'Thing':  [2]
	}
	order := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert order.len == decls.len
	// every declaration belongs to one file (file_id 0), so that file is fully covered
	assert repro_covers_any_whole_file(decls, []ReproImport{}, []ReproHash{}, []ReproImport{},
		order)
}

fn test_repro_covers_any_whole_file_tracks_per_file_coverage() {
	// main.v (file 0) has a single declaration; helpers.v (file 1) has two. The
	// closure includes main (file 0 complete) and one helper (file 1 partial), so
	// order.len < decls.len yet file 0 is fully covered — reject the whole reproducer
	// rather than upload all of main.v (PR #28131 review).
	two_file := [
		ReproDecl{
			names:   ['main']
			source:  'fn main() { helper() }'
			file_id: 0
		},
		ReproDecl{
			names:   ['helper']
			source:  'fn helper() {}'
			file_id: 1
		},
		ReproDecl{
			names:   ['unrelated']
			source:  'fn unrelated() {}'
			file_id: 1
		},
	]
	assert repro_covers_any_whole_file(two_file, []ReproImport{}, []ReproHash{}, []ReproImport{}, [
		0,
		1,
	])
	// No single file is fully covered here (one of two declarations from each file),
	// so the reproducer is a strict subset and is kept.
	partial := [
		ReproDecl{
			names:   ['main']
			source:  'fn main() { helper() }'
			file_id: 0
		},
		ReproDecl{
			names:   ['aux']
			source:  'fn aux() {}'
			file_id: 0
		},
		ReproDecl{
			names:   ['helper']
			source:  'fn helper() {}'
			file_id: 1
		},
		ReproDecl{
			names:   ['unrelated']
			source:  'fn unrelated() {}'
			file_id: 1
		},
	]
	assert !repro_covers_any_whole_file(partial, []ReproImport{}, []ReproHash{}, []ReproImport{}, [
		0,
		2,
	])
}

fn test_repro_covers_any_whole_file_counts_hash_only_files() {
	// main.v (file 0) has two declarations; a declaration-free `.c.v` companion (file 1)
	// contributes only `#flag` directives. The hash loop emits every directive from file
	// 1 regardless of which declarations are kept, so file 1 is always reconstructed
	// whole. Even when only some of file 0's declarations are in the closure, the
	// reproducer must be rejected rather than upload all of the hash-only file
	// (PR #28131 review).
	decls := [
		ReproDecl{
			names:   ['main']
			source:  'fn main() { helper() }'
			file_id: 0
		},
		ReproDecl{
			names:   ['helper']
			source:  'fn helper() {}'
			file_id: 0
		},
	]
	hashes := [
		ReproHash{
			source:  '#flag -DSECRET=hunter2'
			file_id: 1
		},
		ReproHash{
			source:  '#flag -lsecretlib'
			file_id: 1
		},
	]
	// Only `main` from file 0 is retained (file 0 partial), but file 1's every hash
	// directive is emitted, so file 1 is wholly covered.
	assert repro_covers_any_whole_file(decls, []ReproImport{}, hashes, []ReproImport{}, [
		0,
	])
	// Without the hash-only companion, the same partial closure is a strict subset.
	assert !repro_covers_any_whole_file(decls, []ReproImport{}, []ReproHash{}, []ReproImport{}, [
		0,
	])
	// A file carrying both declarations and hashes is only whole-covered when every one
	// of its declarations is also included (its hashes are always emitted).
	mixed_decls := [
		ReproDecl{
			names:   ['main']
			source:  'fn main() { helper() }'
			file_id: 0
		},
		ReproDecl{
			names:   ['helper']
			source:  'fn helper() {}'
			file_id: 0
		},
	]
	mixed_hashes := [
		ReproHash{
			source:  '#flag -lm'
			file_id: 0
		},
	]
	assert !repro_covers_any_whole_file(mixed_decls, []ReproImport{}, mixed_hashes,
		[]ReproImport{}, [0])
	assert repro_covers_any_whole_file(mixed_decls, []ReproImport{}, mixed_hashes, []ReproImport{}, [
		0,
		1,
	])
}

fn test_repro_covers_any_whole_file_counts_import_only_files() {
	decls := [
		ReproDecl{
			names:   ['main']
			source:  'fn main() {}'
			file_id: 0
		},
		ReproDecl{
			names:   ['unused']
			source:  'fn unused() {}'
			file_id: 0
		},
	]
	imports := [
		ReproImport{
			source:      'import private.telemetry as _'
			mod:         'private.telemetry'
			triggers:    ['_']
			side_effect: true
			file_id:     1
		},
	]
	assert repro_covers_any_whole_file(decls, imports, []ReproHash{}, imports, [0])
	assert !repro_covers_any_whole_file(decls, imports, []ReproHash{}, []ReproImport{}, [
		0,
	])
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
	out := repro_render('module main', imports, [], decls, [0])
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
	out := repro_render('module main', imports, [], decls, [0])
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
	out := repro_render('module main', [], ['#include <stdio.h>'], decls, [0])
	assert out.contains('#include <stdio.h>')
}

fn test_repro_is_markused_root() {
	mk := fn (name string, is_method bool, attrs []ast.Attr) ast.FnDecl {
		return ast.FnDecl{
			name:      name
			is_method: is_method
			attrs:     attrs
			scope:     unsafe { nil }
		}
	}
	assert repro_is_markused_root(mk('main.init', false, []), 0, false)
	assert repro_is_markused_root(mk('main.cleanup', false, []), 0, false)
	assert !repro_is_markused_root(mk('main.helper', false, []), 0, false)
	// lifecycle METHODS are roots too: mark-used matches any key ending in `.init`/`.cleanup`
	assert repro_is_markused_root(mk('main.Foo.init', true, []), 0, false)
	assert repro_is_markused_root(mk('main.Foo.cleanup', true, []), 0, false)
	// `@[export]` / `@[markused]` functions are roots
	assert repro_is_markused_root(mk('main.exp', false, [ast.Attr{ name: 'export' }]), 0, false)
	assert repro_is_markused_root(mk('main.mu', false, [ast.Attr{ name: 'markused' }]), 0, false)
	// `lock`/`unlock`/`rlock`/`runlock` methods (shared-type helpers) are roots
	assert repro_is_markused_root(mk('main.Foo.lock', true, []), 0, false)
	assert repro_is_markused_root(mk('main.Foo.unlock', true, []), 0, false)
	assert repro_is_markused_root(mk('main.Foo.rlock', true, []), 0, false)
	assert repro_is_markused_root(mk('main.Foo.runlock', true, []), 0, false)
	// an ordinary method is not a root
	assert !repro_is_markused_root(mk('main.Foo.bar', true, []), 0, false)
	// a plain function named `lock` (not a method) is not a lock helper root
	assert !repro_is_markused_root(mk('main.lock', false, []), 0, false)
}

fn test_repro_is_markused_root_veb_action() {
	// a fn returning `veb.Result` (identified by the table's cached type index) is a
	// router-invoked action: the mark-used pass roots every such function
	assert repro_is_markused_root(ast.FnDecl{
		name:        'main.App.index'
		is_method:   true
		return_type: ast.idx_to_type(123)
		scope:       unsafe { nil }
	}, 123, false)
	assert !repro_is_markused_root(ast.FnDecl{
		name:        'main.App.helper'
		is_method:   true
		return_type: ast.idx_to_type(50)
		scope:       unsafe { nil }
	}, 123, false)
	// no veb in the program: the cache index is unset and roots nothing
	assert !repro_is_markused_root(ast.FnDecl{
		name:      'main.App.index'
		is_method: true
		scope:     unsafe { nil }
	}, 0, false)
}

fn test_repro_hash_is_local() {
	assert !repro_hash_is_local('#include <stdio.h>')
	assert repro_hash_is_local('#include "private.h"')
	assert !repro_hash_is_local('#flag -lssl')
	assert !repro_hash_is_local('#flag darwin -framework Cocoa')
	assert repro_hash_is_local('#flag -I@VMODROOT/c')
	assert repro_hash_is_local('#flag -L./libs -lfoo')
	assert repro_hash_is_local('#flag "./local.o"')
}

fn test_repro_render_preserves_module_attributes() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() {}'
		},
	]
	// a `@[has_globals] module main` header must survive, or `__global` fails the checker
	out := repro_render('@[has_globals]\nmodule main', [], [], decls, [0])
	assert out.starts_with('@[has_globals]\nmodule main')
}

fn test_repro_comptime_decl_names_indexes_nested_decls() {
	block := ast.ExprStmt{
		expr: ast.IfExpr{
			is_comptime: true
			branches:    [
				ast.IfBranch{
					stmts: [
						ast.Stmt(ast.FnDecl{
							name:  'main.linux_only'
							scope: unsafe { nil }
						}),
					]
				},
			]
		}
	}
	names := repro_comptime_decl_names(block)
	assert 'linux_only' in names
	// a plain (non-comptime) statement yields nothing
	assert repro_comptime_decl_names(ast.ExprStmt{ expr: ast.BoolLiteral{} }) == []
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
	out := repro_render('module main', imports, [], decls, [0])
	assert !out.contains('import foo')
}

fn test_repro_comptime_decl_names_indexes_match_decls() {
	// a declaration inside a top-level comptime `$match` is wrapped in an ExprStmt holding a
	// MatchExpr; its name must be indexed so referencing it keeps the whole block
	block := ast.ExprStmt{
		expr: ast.MatchExpr{
			is_comptime: true
			branches:    [
				ast.MatchBranch{
					stmts: [
						ast.Stmt(ast.FnDecl{
							name:  'main.arm_only'
							scope: unsafe { nil }
						}),
					]
				},
			]
		}
	}
	names := repro_comptime_decl_names(block)
	assert 'arm_only' in names
	// a non-comptime match yields nothing
	assert repro_comptime_decl_names(ast.ExprStmt{ expr: ast.MatchExpr{} }) == []
}

fn test_repro_comptime_decl_names_indexes_solved_block() {
	// after `comptime.solve_files` hoists an active single branch, a top-level `$if <os>` block
	// appears as a bare ast.Block; its inner declaration names must still be indexed
	block := ast.Block{
		scope: unsafe { nil }
		stmts: [
			ast.Stmt(ast.FnDecl{
				name:  'main.plat_helper'
				scope: unsafe { nil }
			}),
			ast.Stmt(ast.ConstDecl{
				fields: [ast.ConstField{
					name: 'main.plat_const'
				}]
			}),
		]
	}
	names := repro_comptime_decl_names(block)
	assert 'plat_helper' in names
	assert 'plat_const' in names
}

fn test_repro_decl_attrs_covers_marked_decl_kinds() {
	m := [ast.Attr{ name: 'markused' }]
	assert repro_decl_attrs(ast.ConstDecl{ attrs: m }).any(it.name == 'markused')
	assert repro_decl_attrs(ast.GlobalDecl{ attrs: m }).any(it.name == 'markused')
	assert repro_decl_attrs(ast.StructDecl{ attrs: m }).any(it.name == 'markused')
	assert repro_decl_attrs(ast.InterfaceDecl{ attrs: m }).any(it.name == 'markused')
	assert repro_decl_attrs(ast.EnumDecl{ attrs: m }).any(it.name == 'markused')
	assert repro_decl_attrs(ast.TypeDecl(ast.AliasTypeDecl{ attrs: m })).any(it.name == 'markused')
	// a non-declaration statement carries no attributes
	assert repro_decl_attrs(ast.ExprStmt{ expr: ast.BoolLiteral{} }) == []
}

fn test_repro_hash_is_local_rejects_absolute_flag_paths() {
	// a bare absolute path argument names a file absent on the receiver
	assert repro_hash_is_local('#flag /path/to/ffi.a')
	assert repro_hash_is_local('#flag linux /abs/lib/x.o')
	assert repro_hash_is_local('#flag C:\\libs\\x.a')
	assert repro_hash_is_local('#flag C:/libs/x.a')
	// system libraries and platform-only flags stay non-local
	assert !repro_hash_is_local('#flag -lm')
	assert !repro_hash_is_local('#flag darwin -framework Cocoa')
	assert !repro_hash_is_local('#flag windows -DX=1')
}

fn test_repro_attr_start_walks_over_multiline_attribute() {
	// `@[footer: 'Hello\nWorld']` spans two lines; the line above the struct is a continuation,
	// so the walk must balance brackets back to the `@[` opener on line 0
	lines := ["@[footer: 'Hello", "World']", 'struct Foo {', '\tx int', '}']
	assert repro_attr_start(lines, 2) == 0
	// a preceding declaration ending in `]` (a const array) must NOT be swallowed as an attribute
	lines2 := ['const arr = [1, 2, 3]', 'struct Bar {}']
	assert repro_attr_start(lines2, 1) == 1
	// a multi-line array const above the declaration is likewise preserved
	lines3 := ['const arr = [', '\t1,', '\t2,', ']', 'fn f() {}']
	assert repro_attr_start(lines3, 4) == 4
}

fn test_repro_source_has_toplevel_import() {
	// a retained comptime block that carries its own conditional import is detectable
	block := '\$if linux {\n\timport os\n\tconst x = 1\n}'
	assert repro_source_has_toplevel_import(block)
	// ordinary declarations never contain an import statement
	assert !repro_source_has_toplevel_import('fn main() { println(1) }')
	assert !repro_source_has_toplevel_import('struct Foo {\n\tx int\n}')
}

fn test_repro_reflection_method_targets() {
	t1 := repro_reflection_method_targets('fn f() { \$for m in Foo.methods { } }')
	assert 'Foo' in t1
	// a generic parameter target is captured too
	t2 := repro_reflection_method_targets('fn f[T]() { \$for m in T.methods { } }')
	assert 'T' in t2
	// a lowercase metadata variable from an outer `$for field in C.fields` is captured (so it can
	// trigger the fallback, since `field` is not an inlined type)
	t3 :=
		repro_reflection_method_targets('\$for field in C.fields { \$for m in field.methods { } }')
	assert 'field' in t3
	// `.methods` must be a whole token, so `.methods_count` is not reflection
	assert repro_reflection_method_targets('\$for x in T.methods_count { }') == []
	// a runtime member access on a lowercase variable (no `$for`) is not reflection
	assert repro_reflection_method_targets('fn f(r Registry) { r.methods() }') == []
	// a runtime `.methods()` call inside a `$for` body is a call, not reflection
	assert repro_reflection_method_targets('\$for i in 0 .. 3 { registry.methods() }') == []
	// iterating a runtime `.methods()` result is a call, not reflection
	assert repro_reflection_method_targets('\$for f in T.fields { for x in reg.methods() {} }') == []
	// an ordinary member access after `:=` (not an `in` iterable) is not reflection
	assert repro_reflection_method_targets('fn f() { \$for m in Foo.methods {} \n y := obj.methods }') == [
		'Foo',
	]
	// no reflection at all
	assert repro_reflection_method_targets('fn f() {}') == []
}

fn test_repro_closure_retains_reflected_methods() {
	// `describe` reflects over `Foo.methods`; its methods (indexed under `Foo#methods`) must be
	// pulled into the closure even though their names never appear as identifier tokens
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() { describe(Foo{}) }'
		},
		ReproDecl{
			names:  ['describe']
			source: 'fn describe(f Foo) { \$for m in Foo.methods {} }'
		},
		ReproDecl{
			names:  ['Foo']
			source: 'struct Foo {}'
		},
		ReproDecl{
			names:  ['bar']
			source: 'fn (f Foo) bar() {}'
		},
		ReproDecl{
			names:  ['unrelated']
			source: 'fn unrelated() {}'
		},
	]
	name_to_decl := {
		'main':        [0]
		'describe':    [1]
		'Foo':         [2]
		'bar':         [3]
		'Foo#methods': [3]
		'unrelated':   [4]
	}
	order := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert 3 in order // Foo.bar retained via `.methods` reflection
	assert 4 !in order
}

fn test_repro_source_fn_name() {
	assert repro_source_fn_name('fn compare(a int, b int) int {\n\treturn 0\n}') == 'compare'
	assert repro_source_fn_name('pub fn max[T](a T, b T) T {\n\treturn a\n}') == 'max'
	assert repro_source_fn_name('@[inline]\nfn (mut f Foo) grow[T](x T) {\n}') == 'grow'
	assert repro_source_fn_name('fn C.puts(s &char) int') == 'puts'
	// operator overloads have no source-visible plain name
	assert repro_source_fn_name('fn (a Vec) + (b Vec) Vec {\n\treturn a\n}') == ''
	assert repro_source_fn_name('struct NotAFn {}') == ''
}

fn test_repro_closure_retains_str_methods_for_stringification() {
	// `println('${Foo{}}')` only mentions `Foo` as a token: the implicit `Foo.str` call (and the
	// helper only it reaches) must still be retained, or replaying with default `skip_unused`
	// drops them and the C error vanishes
	decls := [
		ReproDecl{
			names:  ['main']
			source: "fn main() {\n\tprintln('\${Foo{}}')\n}"
		},
		ReproDecl{
			names:  ['Foo']
			source: 'struct Foo {}'
		},
		ReproDecl{
			names:  ['str']
			source: 'fn (f Foo) str() string {\n\treturn str_helper()\n}'
		},
		ReproDecl{
			names:  ['str_helper']
			source: "fn str_helper() string {\n\treturn 'foo'\n}"
		},
		ReproDecl{
			names:  ['unrelated']
			source: 'fn unrelated() {}'
		},
	]
	mut name_to_decl := map[string][]int{}
	for i, d in decls {
		for n in d.names {
			name_to_decl[n] << i
		}
	}
	ordered := repro_closure(decls, name_to_decl, [0]) or {
		assert false
		return
	}
	assert 0 in ordered
	assert 1 in ordered
	assert 2 in ordered // Foo.str, referenced only implicitly by the interpolation
	assert 3 in ordered // reachable only through Foo.str
	assert 4 !in ordered
}

fn test_repro_source_stringifies() {
	assert repro_source_stringifies("fn f() {\n\tprintln('\${x}')\n}", [])
	assert repro_source_stringifies('fn f() {}', ['println'])
	assert repro_source_stringifies('fn f() {}', ['dump'])
	assert repro_source_stringifies('fn f() {}', ['assert'])
	assert !repro_source_stringifies("fn f() {\n\ty := 'plain'\n}", ['f', 'y'])
}

fn test_repro_import_collides() {
	included := {
		0: true
		1: true
	}
	// file 0 imports `x`; file 1 uses `x` as a parameter/local without importing it
	mut referenced := {
		'0\x00x': true
		'1\x00x': true
	}
	mut binds := {
		'0\x00x': true
	}
	assert repro_import_collides('x', 0, included, referenced, binds)
	// once file 1 carries an equivalent import, its uses are references, not bindings
	binds['1\x00x'] = true
	assert !repro_import_collides('x', 0, included, referenced, binds)
	// a name unused outside the importing file cannot collide
	assert !repro_import_collides('y', 0, included, referenced, binds)
}

fn test_repro_uses_local_resource_env() {
	assert repro_uses_local_resource("const build_host = \$env('HOSTNAME')")
	assert !repro_uses_local_resource("fn f() {\n\tx := os.getenv('HOSTNAME')\n}")
}

fn test_repro_is_markused_root_before_request() {
	assert repro_is_markused_root(ast.FnDecl{ name: 'main.App.before_request', is_method: true },
		0, false)
	assert repro_is_markused_root(ast.FnDecl{ name: 'main.before_request' }, 0, false)
	// mark-used matches by suffix (`k.ends_with('before_request')`), so a `*_before_request` name
	// that the original build retains must be seeded too
	assert repro_is_markused_root(ast.FnDecl{
		name:      'main.App.admin_before_request'
		is_method: true
	}, 0, false)
	assert !repro_is_markused_root(ast.FnDecl{ name: 'main.App.handle', is_method: true }, 0, false)
	assert !repro_is_markused_root(ast.FnDecl{ name: 'main.before_request_handler' }, 0, false)
}

fn test_repro_has_comptime_call_allows_spacing() {
	assert repro_has_comptime_call("x := \$embed_file ('asset.bin')", 'embed_file')
	assert repro_has_comptime_call("x := \$env\t('HOME')", 'env')
	assert repro_uses_local_resource("t := \$tmpl ('page.html')")
	assert repro_uses_local_resource("r := \$res ('icon')")
	// `$reserve(` must not be mistaken for `$res`
	assert !repro_has_comptime_call('x := \$reserve(3)', 'res')
	// `$env` without a call is not the compile-time function
	assert !repro_has_comptime_call('// mentions \$env only', 'env')
}

fn test_repro_uses_machine_pseudo() {
	assert repro_uses_machine_pseudo('const here = @FILE')
	assert repro_uses_machine_pseudo('fn f() int {\n\treturn @LINE.int()\n}')
	assert repro_uses_machine_pseudo("const root = @VMODROOT + '/data'")
	assert repro_uses_machine_pseudo('const built = @BUILD_TIMESTAMP')
	// identifier boundary: not a pseudo variable
	assert !repro_uses_machine_pseudo("const mail = 'bob@FILEserver.example'")
	// stable pseudos survive the verbatim flatten and recorded build options
	assert !repro_uses_machine_pseudo('fn f() {\n\tprintln(@FN)\n\tprintln(@OS)\n}')
}

fn test_repro_bracket_delta_skips_strings_and_comments() {
	assert repro_bracket_delta('@[footer:eval]') == 0
	assert repro_bracket_delta("@[footer: 'Hello\\nWorld]']") == 0
	assert repro_bracket_delta("@[deprecated: 'use y[0]']") == 0
	assert repro_bracket_delta('@[unsafe] // note]') == 0
	assert repro_bracket_delta('x := arr[') == 1
	assert repro_bracket_delta(']') == -1
}

fn test_repro_attr_start_with_bracket_in_attr_string() {
	lines := [
		'module main',
		'',
		"@[footer: 'Hello\\nWorld]']",
		'struct Config {}',
	]
	assert repro_attr_start(lines, 3) == 2
	// the combined case: the attribute string spans source lines AND contains brackets
	lines2 := ["@[cfg: 'a[b", "c']", 'fn f() {}']
	assert repro_attr_start(lines2, 2) == 0
	// an unrelated attribute above ordinary code is still not treated as this group's opener
	lines3 := ['@[inline]', 'fn a() {}', 'const c = [', '1,', ']', 'fn b() {}']
	assert repro_attr_start(lines3, 5) == 5
}

fn test_repro_source_fn_name_multiline_attribute() {
	// a multiline attribute value must be skipped whole: its continuation line is not the
	// declaration line, and the generic fn behind it must still be indexed under `pick`
	src := "@[footer: 'Hello\nWorld']\npub fn pick[T](a T, b T) T {\n\treturn a\n}"
	assert repro_source_fn_name(src) == 'pick'
	// a bracket inside the attribute string must not derail the group scan either
	src2 := "@[cfg: 'a[b\nc]']\nfn choose(a int) int {\n\treturn a\n}"
	assert repro_source_fn_name(src2) == 'choose'
	// stacked single-line attributes still skip correctly
	src3 := '@[inline]\n@[direct_array_access]\nfn fast() {}'
	assert repro_source_fn_name(src3) == 'fast'
}

fn test_repro_source_iterates() {
	assert repro_source_iterates('fn f() {\n\tfor item in Iterator{} {\n\t\tprintln(item)\n\t}\n}')
	assert repro_source_iterates('fn f() {\n\tfor i, x in arr {\n\t}\n}')
	// C-style and condition loops do not iterate a container
	assert !repro_source_iterates('fn f() {\n\tfor i := 0; i < 3; i++ {\n\t}\n}')
	assert !repro_source_iterates('fn f() {\n\tfor running {\n\t}\n}')
	// `$for` iterates compile-time metadata, not values
	assert !repro_source_iterates('fn f() {\n\t\$for m in Foo.methods {\n\t}\n}')
	// `informal` must not be mistaken for the `in` keyword, nor `xfor` for `for`
	assert !repro_source_iterates('fn f() {\n\tfor informal() {\n\t}\n}')
	assert !repro_source_iterates('fn f() {\n\txfor in y {\n\t}\n}')
}

fn test_repro_closure_retains_next_for_iteration() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() {\n\tfor item in Iter{} {\n\t\tprintln(item)\n\t}\n}'
		},
		ReproDecl{
			names:  ['Iter']
			source: 'struct Iter {\nmut:\n\ti int\n}'
		},
		ReproDecl{
			names:  ['next']
			source: 'fn (mut it Iter) next() ?int {\n\treturn iter_helper(it.i)\n}'
		},
		ReproDecl{
			names:  ['iter_helper']
			source: 'fn iter_helper(i int) ?int {\n\treturn none\n}'
		},
		ReproDecl{
			names:  ['unrelated']
			source: 'fn unrelated() {}'
		},
	]
	mut name_to_decl := map[string][]int{}
	for i, d in decls {
		for n in d.names {
			name_to_decl[n] << i
		}
	}
	ordered := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert 2 in ordered // Iter.next, invoked only implicitly by the for-in loop
	assert 3 in ordered // reachable only through next
	assert 4 !in ordered
}

fn test_repro_is_markused_root_translated_c_attr() {
	// a -translated build roots every function carrying a `c` attribute
	assert repro_is_markused_root(ast.FnDecl{
		name:  'main.wrapped'
		attrs: [ast.Attr{ name: 'c', arg: 'real_name' }]
		scope: unsafe { nil }
	}, 0, true)
	// without -translated the attribute alone is not a root
	assert !repro_is_markused_root(ast.FnDecl{
		name:  'main.wrapped'
		attrs: [ast.Attr{ name: 'c', arg: 'real_name' }]
		scope: unsafe { nil }
	}, 0, false)
}

fn test_repro_closure_retains_json_hooks_for_encode() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() {\n\tprintln(json2.encode(Foo{}))\n}'
		},
		ReproDecl{
			names:  ['Foo']
			source: 'struct Foo {}'
		},
		ReproDecl{
			names:  ['to_json']
			source: 'fn (f Foo) to_json() string {\n\treturn json_helper()\n}'
		},
		ReproDecl{
			names:  ['json_helper']
			source: "fn json_helper() string {\n\treturn '{}'\n}"
		},
		ReproDecl{
			names:  ['unrelated']
			source: 'fn unrelated() {}'
		},
	]
	mut name_to_decl := map[string][]int{}
	for i, d in decls {
		for n in d.names {
			name_to_decl[n] << i
		}
	}
	ordered := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert 2 in ordered // Foo.to_json, selected only implicitly by json2.encode
	assert 3 in ordered // reachable only through the hook
	assert 4 !in ordered
}

fn test_repro_uses_local_resource_veb_html() {
	// `$veb.html()` resolves and reads a project-local template at parse time
	assert repro_uses_local_resource('fn (mut app App) page() veb.Result {\n\treturn \$veb.html()\n}')
	assert repro_uses_local_resource("return \$veb.html ('views/index.html')")
	assert !repro_uses_local_resource("s := 'mentions veb.html in text'")
}

fn test_repro_embedded_local_hash() {
	// a retained comptime block can embed its own directive; local ones cannot be satisfied
	assert repro_embedded_local_hash('\$if linux {\n\t#include "private.h"\n}')
	assert repro_embedded_local_hash('\$if freebsd {\n\t#flag ./local.o\n}')
	// system headers and libraries embedded in a block are uploadable as-is
	assert !repro_embedded_local_hash('\$if linux {\n\t#include <sys/epoll.h>\n}')
	assert !repro_embedded_local_hash('\$if linux {\n\t#flag -lm\n}')
	assert !repro_embedded_local_hash('fn plain() {}')
}

fn test_repro_closure_retains_operator_methods_of_used_types() {
	// `Vec` is used without any source-level `+`: the mark-used finalizer still roots `Vec.+`,
	// so the closure must retain it (and the helper only it reaches) via the `Vec#op` key
	decls := [
		ReproDecl{
			names:  ['main']
			source: 'fn main() {\n\tv := Vec{}\n\tprintln(v)\n}'
		},
		ReproDecl{
			names:  ['Vec']
			source: 'struct Vec {}'
		},
		ReproDecl{
			names:  ['+']
			source: 'fn (a Vec) + (b Vec) Vec {\n\treturn op_helper(a, b)\n}'
		},
		ReproDecl{
			names:  ['op_helper']
			source: 'fn op_helper(a Vec, b Vec) Vec {\n\treturn a\n}'
		},
	]
	mut name_to_decl := map[string][]int{}
	for i, d in decls {
		for n in d.names {
			name_to_decl[n] << i
		}
	}
	name_to_decl['Vec#op'] << 2
	ordered := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert 2 in ordered // Vec.+, rooted by the used receiver type
	assert 3 in ordered // reachable only through the operator method
}

fn test_repro_source_has_hash_directive() {
	assert repro_source_has_hash_directive('\$if linux {\n\t#flag -DFOO\n}')
	assert repro_source_has_hash_directive('\$if windows {\n\t#include <windows.h>\n}')
	assert !repro_source_has_hash_directive('\$if linux {\n\tprintln(1)\n}')
}

fn test_repro_uses_local_resource_pkgconfig() {
	// package availability is evaluated on the build machine; replaying elsewhere can select
	// the opposite branch
	assert repro_uses_local_resource("\$if \$pkgconfig('openssl') {\n\tx()\n}")
	// the `#pkgconfig` directive form resolves on the receiver like a system library
	assert !repro_uses_local_resource('#pkgconfig openssl')
}

fn test_repro_merge_module_imports() {
	// a side-effect import is subsumed by a real import of the same module
	merged := repro_merge_module_imports([
		ReproImport{
			source: 'import log'
			mod:    'log'
		},
		ReproImport{
			source:      'import log as _'
			mod:         'log'
			side_effect: true
		},
	]) or {
		assert false, 'merge unexpectedly failed'
		return
	}
	assert merged.map(it.source) == ['import log']
	// exact duplicates collapse
	dup := repro_merge_module_imports([
		ReproImport{
			source: 'import os'
			mod:    'os'
		},
		ReproImport{
			source: 'import os'
			mod:    'os'
		},
	]) or {
		assert false, 'merge unexpectedly failed'
		return
	}
	assert dup.len == 1
	// side-effect imports with no real counterpart stay
	side := repro_merge_module_imports([
		ReproImport{
			source:      'import sqlite as _'
			mod:         'sqlite'
			side_effect: true
		},
	]) or {
		assert false, 'merge unexpectedly failed'
		return
	}
	assert side.map(it.source) == ['import sqlite as _']
	// genuinely different forms of one module cannot be flattened
	mut conflicted := false
	if _ := repro_merge_module_imports([
		ReproImport{
			source: 'import log'
			mod:    'log'
		},
		ReproImport{
			source: 'import log as l'
			mod:    'log'
		},
	])
	{
		conflicted = false
	} else {
		conflicted = true
	}
	assert conflicted
}

fn test_repro_fn_in_used_fns() {
	assert repro_fn_in_used_fns('main.entry', ['main.entry'])
	assert repro_fn_in_used_fns('main.handle_thing', ['main.handle_*'])
	assert !repro_fn_in_used_fns('main.other', ['main.entry', 'main.handle_*'])
	assert !repro_fn_in_used_fns('main.entry', [])
}

fn test_repro_block_opener_line_nested_blocks() {
	// `$if linux { $if linux { fn helper() {} } }`: both solved blocks have unset positions,
	// so the outer block must descend into the inner one to find a usable line
	lines := ['$if linux {', '\t$if linux {', '\t\tfn helper() {}', '\t}', '}']
	inner := ast.Block{
		scope: unsafe { nil }
		stmts: [
			ast.Stmt(ast.FnDecl{
				name:  'main.helper'
				pos:   token.Pos{
					line_nr: 2
				}
				scope: unsafe { nil }
			}),
		]
	}
	outer := ast.Block{
		scope: unsafe { nil }
		stmts: [ast.Stmt(inner)]
	}
	assert repro_block_opener_line(outer, lines) == 0
	// an empty nested chain still reports no opener
	empty := ast.Block{
		scope: unsafe { nil }
		stmts: [
			ast.Stmt(ast.Block{
				scope: unsafe { nil }
			}),
		]
	}
	assert repro_block_opener_line(empty, lines) == -1
}

fn test_repro_attr_start_over_trivia() {
	// the parser attaches an attribute group across blank and comment lines
	lines := ['@[markused]', '', 'fn keep() {}']
	assert repro_attr_start(lines, 2) == 0
	lines2 := ['@[export]', '// exported for the C side', 'fn exp() {}']
	assert repro_attr_start(lines2, 2) == 0
	// a const array above a blank line is still not swallowed as an attribute
	lines3 := ['const arr = [1, 2]', '', 'fn f() {}']
	assert repro_attr_start(lines3, 2) == 2
}

fn test_repro_hash_is_local_include_delimiter() {
	// only the first delimiter after the keyword decides; trailing comments do not
	assert repro_hash_is_local('#include "private.h" // see <API>')
	assert !repro_hash_is_local('#include <stdio.h> // has "quotes"')
	assert !repro_hash_is_local('#include   <sys/epoll.h>')
	assert repro_hash_is_local('#include "local.h"')
}

fn test_repro_closure_retains_json_decode_hooks() {
	decls := [
		ReproDecl{
			names:  ['main']
			source: "fn main() {\n\tf := json2.decode[Foo]('{}') or { Foo{} }\n\tprintln(f)\n}"
		},
		ReproDecl{
			names:  ['Foo']
			source: 'struct Foo {}'
		},
		ReproDecl{
			names:  ['from_json_string']
			source: 'fn (mut f Foo) from_json_string(raw string) ! {\n\tdecode_helper(raw)!\n}'
		},
		ReproDecl{
			names:  ['decode_helper']
			source: 'fn decode_helper(raw string) ! {\n}'
		},
	]
	mut name_to_decl := map[string][]int{}
	for i, d in decls {
		for n in d.names {
			name_to_decl[n] << i
		}
	}
	ordered := repro_closure(decls, name_to_decl, [0]) or {
		assert false, 'closure returned none'
		return
	}
	assert 2 in ordered // Foo.from_json_string, selected implicitly by the decoder
	assert 3 in ordered // reachable only through the hook
}

fn test_repro_flag_has_bare_file() {
	assert repro_flag_has_bare_file('#flag helper.o')
	assert repro_flag_has_bare_file('#flag linux extra.a')
	assert !repro_flag_has_bare_file('#flag -lssl')
	assert !repro_flag_has_bare_file('#flag windows -lgdi32')
	assert !repro_flag_has_bare_file('#flag -DVERSION=1.2')
	// locality classification end to end
	assert repro_hash_is_local('#flag -include config.h')
	assert repro_hash_is_local('#flag helper.o')
	assert !repro_hash_is_local('#flag -lm')
}
