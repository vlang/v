module parser

import os
import v.pref
import v.token

struct SizeofTables {
	types   []string
	consts  []string
	globals []string
	tokens  int
}

fn sizeof_tables_of(path string, module_name string, source string, collapse bool) SizeofTables {
	mut p := Parser.new(pref.new_preferences())
	p.cur_file = path
	p.cur_module = module_name
	mut file_set := token.FileSet.new()
	file := file_set.add_file(path, source.len)
	p.s.init(file, source)
	mut tokens := []InlineAsmScanToken{}
	sizeof_declaration_tokens(p.prefs, file, source, collapse, mut tokens)
	p.scan_translated_sizeof_range(source, tokens, 0, tokens.len)
	mut types := p.translated_sizeof_type_names.keys()
	mut consts := p.translated_sizeof_const_names.keys()
	mut globals := p.translated_sizeof_global_names.keys()
	types.sort()
	consts.sort()
	globals.sort()
	return SizeofTables{
		types:   types
		consts:  consts
		globals: globals
		tokens:  tokens.len
	}
}

fn source_module_name(source string) string {
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('module ') {
			return trimmed.all_after('module ').trim_space()
		}
	}
	return 'main'
}

fn assert_same_sizeof_tables(path string, source string) (int, int) {
	module_name := source_module_name(source)
	full := sizeof_tables_of(path, module_name, source, false)
	collapsed := sizeof_tables_of(path, module_name, source, true)
	assert collapsed.types == full.types, path
	assert collapsed.consts == full.consts, path
	assert collapsed.globals == full.globals, path
	return full.tokens, collapsed.tokens
}

// Collapsing the blocks that the sizeof declaration scan never reads must not
// change what it finds, for every compiler source.
fn test_collapsed_sizeof_tokens_index_like_all_tokens() {
	vlib := os.dir(os.dir(os.dir(@FILE)))
	mut dirs := ['builtin', 'os', 'strings', 'strconv', 'time', 'sync']
	for sub in ['types', 'transform', 'gen/c', 'parser', 'driver', 'markused', 'flat', 'scanner',
		'token', 'pref', 'util'] {
		dirs << 'v/' + sub
	}
	mut full_tokens := 0
	mut collapsed_tokens := 0
	mut files := 0
	for dir in dirs {
		path := os.join_path(vlib, dir)
		mut names := os.ls(path) or { continue }
		names.sort()
		for name in names {
			if !name.ends_with('.v') || name.ends_with('_test.v') {
				continue
			}
			file := os.join_path(path, name)
			source := os.read_file(file) or { continue }
			full, collapsed := assert_same_sizeof_tables(file, source)
			full_tokens += full
			collapsed_tokens += collapsed
			files++
		}
	}
	assert files > 100
	// The collapse has to happen at all for this test to mean anything.
	assert collapsed_tokens * 4 < full_tokens
}

// Blocks whose contents the scan reads differently from their braces keep
// their tokens: comptime branches and arms, unbalanced brackets, attributes and
// literals next to identifiers.
fn test_collapsed_sizeof_tokens_keep_the_blocks_the_scan_reads() {
	sources := [
		'module m\n\$if linux {\n\tconst a = 1\n} \$else {\n\tconst b = 2\n}\nfn f() { x := 1 }\n',
		'module m\n\$match os {\n\t"linux" { const c = 1 }\n\telse { struct S {} }\n}\n',
		'module m\nconst (\n\ta = Foo{ x: (1 }\n\tb = 2\n)\nconst c = [1 }\nconst d = 3\n',
		'module m\nconst e = Foo{ g: [1, 2] }\nconst f = 4\n',
		"module m\nstruct T {\n\t@[json: 'x']\n\tx int\n}\ntype U = int\n",
		"module m\nfn g() {\n\ts := r'}'\n\tt := c'{'\n\tu := 'a\${b}c'\n}\nconst h = 5\n",
		'module m\nfn h() {\n\t// } {\n\t/* { */\n\tx := `}`\n}\n__global ( gg int )\n',
		"module m\nfn i() { foo'x' }\nconst j = 6\n",
		'module m\n\$if x == Foo{} {\n\tconst k = 7\n}\nconst l = 8\n',
		'module m\nconst m = \$if linux { 1 } \$else { 2 }\nunion V {}\n',
		'module m\nfn n() {\n\ta := 1\n\tb := (a)\n} const o = 9\n',
	]
	for i, source in sources {
		assert_same_sizeof_tables('/tmp/sizeof_${i}.v', source)
	}
}
