import os
import v.parser
import v.pref

fn test_uppercase_fixed_array_fields_are_not_embeds() {
	source_path := os.join_path(os.temp_dir(), 'v3_uppercase_fixed_array_field_${os.getpid()}.v')
	os.write_file(source_path, 'pub struct C.ImGuiLike {\npub mut:\n\tColors [2]f32\n\tKeysData [3]i32\n}\n') or {
		panic(err)
	}
	defer {
		os.rm(source_path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_into(source_path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut fields := map[string]string{}
	for node in p.a.nodes {
		if node.kind == .field_decl {
			fields[node.value] = node.typ
		}
	}
	assert fields['Colors'] == '[2]f32'
	assert fields['KeysData'] == '[3]i32'
}
