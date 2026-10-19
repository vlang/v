module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_contextual_map_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('contextual_map.v', source.len), source)
	mut tokens := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] { break }
		tokens << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return tokens
}

fn test_fastc_contextual_empty_map_only_retypes_complete_by_value_literals() {
	prefs := pref.new_preferences()
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Ids', 'Map_u64_bool')
	g := Parser{
		prefs:            prefs
		s:                scanner.new_scanner(prefs, .normal)
		selfhost:         true
		alias_base_types: aliases
	}
	expected := flat.source_type_id('Map_u64_bool')
	for source in ['map[string]bool{}', '((map[string]bool{}))'] {
		tokens := fastc_contextual_map_tokens(source)
		for target in [expected, flat.source_type_id('Ids')] {
			value := g.render_map_literal_argument(tokens, target) or { panic('missing contextual map') }
			assert value.typ == target
			assert value.source.contains('sizeof(u64), sizeof(bool)')
			assert value.source.contains('&builtin__map_hash_int_8')
		}
	}
	for source in ['map[u64]bool{}', 'values', 'map[string]bool{"key": true}',
		'(map[string]map[u64]int{})["missing"]'] {
		assert g.render_contextual_empty_map_initializer(fastc_contextual_map_tokens(source), expected) == none
	}
	for target in [u32(0), flat.source_type_int, flat.source_type_id('Map_u64_bool*')] {
		assert g.render_contextual_empty_map_initializer(fastc_contextual_map_tokens('map[string]bool{}'), target) == none
	}
	// A recursive receiver renderer must retain its own literal's layout even
	// when the surrounding expression expects a different map type.
	ambient := Parser{
		prefs:                    prefs
		s:                        scanner.new_scanner(prefs, .normal)
		selfhost:                 true
		expected_expression_type: flat.source_type_id('Map_u64_int')
	}
	receiver := ambient.render_map_expression(fastc_contextual_map_tokens('map[string]map[u64]int{}')) or {
		panic('missing nested map receiver')
	}
	assert receiver.typ != ambient.expected_expression_type
	assert receiver.source.contains('&builtin__map_hash_string')
	ordinary := Parser{
		prefs: prefs
		s:     scanner.new_scanner(prefs, .normal)
	}
	assert ordinary.render_contextual_empty_map_initializer(fastc_contextual_map_tokens('map[string]bool{}'), expected) == none
}

fn test_fastc_contextual_empty_maps_preserve_key_and_value_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_contextual_empty_map_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Cache {
mut:
	numeric map[u64]bool
	narrow map[u32]i8
	strings map[string]u64
}
type Ids = map[u64]bool
struct AliasHolder { mut: entries Ids }

fn from_return() map[u64]bool {
	return map[string]bool{}
}
fn from_argument(input map[u64]bool) map[u64]bool {
	mut values := input.clone()
	values[u64(42)] = true
	return values
}
fn main() {
	mut named := Cache{
		numeric: map[string]bool{}
		narrow: map[string]int{}
		strings: map[u64]u8{}
	}
	named.numeric[u64(0xfedcba9876543210)] = true
	named.narrow[u32(7)] = i8(-5)
	named.strings["answer"] = u64(0xfedcba9876543210)
	assert named.numeric[u64(0xfedcba9876543210)]
	assert named.narrow[u32(7)] == i8(-5)
	assert named.strings["answer"] == u64(0xfedcba9876543210)
	mut positional := Cache{map[string]bool{}, map[string]int{}, map[u64]u8{}}
	positional.numeric[u64(42)] = true
	assert positional.numeric[u64(42)]
	mut aliased := AliasHolder{entries: map[string]bool{}}
	alias_values := from_argument(aliased.entries)
	assert alias_values[u64(42)]
	mut returned := from_return()
	returned[u64(42)] = true
	assert returned[u64(42)]
	argument := from_argument((map[string]bool{}))
	assert argument[u64(42)]
	mut assigned := map[u64]bool{}
	assigned = map[string]bool{}
	assigned[u64(42)] = true
	assert assigned[u64(42)]
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
