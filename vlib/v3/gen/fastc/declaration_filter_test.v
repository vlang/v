module fastc

import strings
import v3.pref

fn test_filtered_declaration_sources_preserve_locations() {
	prefs := pref.new_preferences()
	path := 'filtered_declarations.v'
	source := "module main

fn before() {
	println('const fake = 0')
}

@[flag]
enum Mode {
	one
	two
}

struct Box {
	value int
}

const answer = @LINE
const values = [1, 2, 3]

fn after() {}
"
	partial := fastc_collect_declaration_chunk([
		FastcSourceFile{
			path: path
			source: source
			header: FastcSourceHeader{
				module_name: 'main'
				has_constants: true
			}
		},
	], prefs, 0, 1)
	assert !partial.failed, partial.error_message
	type_source := partial.type_sources[path]
	constant_source := partial.constant_sources[path]
	assert type_source.contains('@[flag]\nenum Mode')
	assert type_source.contains('struct Box')
	assert !type_source.contains('fn before')
	assert !type_source.contains('const fake')
	assert constant_source.contains('const answer = @LINE')
	assert constant_source.contains('const values = [1, 2, 3]')
	assert !constant_source.contains('fn before')
	assert !constant_source.contains('const fake')
	original_position := source.index('@LINE') or { -1 }
	filtered_position := constant_source.index('@LINE') or { -1 }
	assert original_position >= 0
	assert filtered_position >= 0
	original_line, original_column := fastc_line_column(source, original_position)
	filtered_line, filtered_column := fastc_line_column(constant_source, filtered_position)
	assert filtered_line == original_line
	assert filtered_column == original_column
}

fn test_filtered_eof_comptime_spans_include_closing_brace() {
	prefs := pref.new_preferences()
	path := 'filtered_eof_comptime.v'
	source := 'module main\n\n\$if true {\nstruct Tail {}\nconst tail = 1\n}'
	assert !source.ends_with('\n')
	partial := fastc_collect_declaration_chunk([
		FastcSourceFile{
			path: path
			source: source
			header: FastcSourceHeader{
				module_name: 'main'
				has_constants: true
			}
		},
	], prefs, 0, 1)
	assert !partial.failed, partial.error_message
	assert partial.type_sources[path].ends_with('}')
	assert partial.constant_sources[path].ends_with('}')
}

fn test_constant_visibility_resets_after_each_declaration() {
	prefs := pref.new_preferences()
	path := 'constant_visibility.v'
	source := 'module example\n\npub const public_one = 1\nconst private_one = 2\npub const (\n\tpublic_group = 3\n)\nconst private_after_group = 4\n'
	partial := fastc_collect_declaration_chunk([
		FastcSourceFile{
			path: path
			source: source
			header: FastcSourceHeader{
				module_name: 'example'
				has_constants: true
			}
		},
	], prefs, 0, 1)
	assert !partial.failed, partial.error_message
	assert 'example.public_one' in partial.public_constants
	assert 'example.public_group' in partial.public_constants
	assert 'example.private_one' !in partial.public_constants
	assert 'example.private_after_group' !in partial.public_constants
}

fn test_partitioned_c_directives_match_materialized_hoisting() {
	for source in [
		'one\n#include <x.h>\ntwo\n# if FLAG\nthree\n#ifdef INNER\nfour\n#endif\n#else\nfive\n#endif\nsix',
		'one\n#include <x.h>',
		'one\n#if FLAG\ntwo\n#endif',
	] {
		hoisted := fastc_hoist_c_directives(source)
		partitioned := fastc_partition_c_directives(source)
		event_partitioned := fastc_partition_c_directive_lines(source, fastc_scan_c_directive_lines(source))
		assert event_partitioned.final_kind == partitioned.final_kind
		mut event_directives := strings.new_builder(256)
		fastc_write_c_source_ranges(mut event_directives, event_partitioned.source, event_partitioned.directive_ranges)
		mut event_conditional_code := strings.new_builder(256)
		fastc_write_c_source_ranges(mut event_conditional_code, event_partitioned.source, event_partitioned.conditional_ranges)
		mut event_body := strings.new_builder(source.len)
		fastc_write_c_source_ranges(mut event_body, event_partitioned.source, event_partitioned.body_ranges)
		mut directives := strings.new_builder(256)
		fastc_write_c_source_ranges(mut directives, partitioned.source, partitioned.directive_ranges)
		assert event_directives.str() == directives.str()
		if partitioned.final_kind == 1 {
			directives.write_u8(`\n`)
		}
		if partitioned.directive_ranges.len > 0 {
			directives.writeln('')
		}
		mut conditional_code := strings.new_builder(256)
		fastc_write_c_source_ranges(mut conditional_code, partitioned.source, partitioned.conditional_ranges)
		assert event_conditional_code.str() == conditional_code.str()
		if partitioned.final_kind == 2 {
			conditional_code.write_u8(`\n`)
		}
		if partitioned.conditional_ranges.len > 0 {
			conditional_code.writeln('')
		}
		mut body := strings.new_builder(source.len)
		fastc_write_c_source_ranges(mut body, partitioned.source, partitioned.body_ranges)
		assert event_body.str() == body.str()
		if partitioned.final_kind == 0 {
			body.write_u8(`\n`)
		}
		assert directives.str() == hoisted.directives
		assert conditional_code.str() == hoisted.conditional_code
		assert body.str() == hoisted.body
	}
}
