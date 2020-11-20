// import v.table
import v.doc

// fn test_generate_with_pos() {}
// fn test_generate() {}
// fn test_generate_from_ast() {}
fn test_generate_from_mod() {
	nested_mod_name := 'net.http.chunked'
	nested_mod_doc := doc.generate_from_mod(nested_mod_name, false, true) or {
		eprintln(err)
		assert false
		doc.Doc{}
	}
	assert nested_mod_doc.head.name == nested_mod_name
	assert nested_mod_doc.head.content == 'module $nested_mod_name'
	assert nested_mod_doc.contents.len == 3
	assert nested_mod_doc.contents['ChunkScanner'].children.len == 3
}
