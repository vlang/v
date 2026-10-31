module modulecache

import os
import v.flat
import v.parser
import v.pref

fn test_cached_struct_fields_preserve_named_types_embeds_and_attributes() ! {
	root := os.join_path(os.vtmp_dir(), 'cached_struct_fields_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'source.v')
	os.write_file(source, 'module fields
pub struct Base { value int }
pub struct Fields {
	Base
pub mut:
	i64 i64 @[json: "number"]
	string string @[deprecated: "use text"]
	bool bool
	u64 u64
	f32 f32
	f64 f64
	int int
	i8 i8
	u8 u8
	i16 i16
}
struct Private { string string }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut headers := []string{}
	mut fields := []flat.Node{}
	for node in a.nodes {
		if node.kind == .struct_decl {
			headers << struct_text(a, node, []string{}, true)
		} else if node.kind == .field_decl {
			fields << node
		}
	}
	assert fields.len == 13
	header := os.join_path(root, 'cached.vh')
	os.write_file(header, 'module fields\n' + headers.join('\n'))!
	mut cached_parser := parser.Parser.new(pref.new_preferences())
	cached := cached_parser.parse_file(header)
	assert cached_parser.diagnostics.len == 0, cached_parser.diagnostics.str()
	mut index := 0
	for node in cached.nodes {
		if node.kind != .field_decl { continue }
		original := fields[index]
		assert node.value == original.value
		assert node.typ == original.typ
		assert node.generic_params() == original.generic_params()
		index++
	}
	assert index == fields.len
}
