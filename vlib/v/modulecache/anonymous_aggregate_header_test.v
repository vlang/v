module modulecache

import os
import v.parser
import v.pref

// A header spells out the anonymous structs of the declarations it writes: the
// names the parser makes up for them have no declaration a dependent could read.
fn test_header_spells_out_anonymous_struct_types() ! {
	root := os.join_path(os.vtmp_dir(), 'cached_anonymous_aggregates_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'source.v')
	os.write_file(source, 'module foo

pub fn bar(params struct { name string }) string {
	return params.name
}

pub struct Holder {
pub:
	inner struct {
		count int = 3
		nested struct { flag bool }
	}
}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut source_cache := map[string]string{}
	sources := anonymous_aggregate_sources(a, mut source_cache)
	mut texts := []string{}
	for node in a.nodes {
		if node.kind == .fn_decl && node.value == 'bar' {
			texts << fn_text(a, 'foo', node, false, []string{}, true)
		} else if node.kind == .struct_decl && node.value == 'Holder' {
			texts << struct_text(a, node, []string{}, true)
		}
	}
	assert texts.len == 2
	mut spelled := []string{}
	for text in texts {
		assert text.contains('AnonStruct_'), text
		spelled << spell_anonymous_aggregates(text, sources)
	}
	assert spelled[0].contains('params struct { name string }'), spelled[0]
	assert !spelled[0].contains('AnonStruct_'), spelled[0]
	assert spelled[1].contains('count int = 3'), spelled[1]
	assert spelled[1].contains('nested struct { flag bool }'), spelled[1]
	assert !spelled[1].contains('AnonStruct_'), spelled[1]
	header := os.join_path(root, 'cached.vh')
	os.write_file(header, 'module foo\n\n' + spelled.join('\n\n') + '\n')!
	mut cached_parser := parser.Parser.new(pref.new_preferences())
	cached_parser.parse_file(header)
	assert cached_parser.diagnostics.len == 0, cached_parser.diagnostics.str()
}

fn test_anonymous_aggregate_names_are_replaced_as_whole_names() {
	sources := {
		'AnonStruct_x_1': 'struct { a int }'
	}
	assert spell_anonymous_aggregates('fn f(a AnonStruct_x_1, b AnonStruct_x_10) []AnonStruct_x_1',
		sources) == 'fn f(a struct { a int }, b AnonStruct_x_10) []struct { a int }'
	assert spell_anonymous_aggregates('MyAnonStruct_x_1', sources) == 'MyAnonStruct_x_1'
}
