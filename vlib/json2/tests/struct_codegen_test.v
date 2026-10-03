import os

// These tests check which json2 decoders get specialized for a user struct, since that
// code is generated again for every struct that an application decodes (issue #29354).
const vexe = @VEXE

const plain_struct_program = 'import json2

struct Plain {
	id   int
	name string
	tags []string
	note ?string
}

fn main() {
	println(json2.encode(Plain{ id: 1 }))
	println(json2.decode[Plain](\'{"id":2,"name":"x"}\') or { Plain{} })
}
'

const embedding_struct_program = 'import json2

struct Base {
	id int
}

struct Child {
	Base
	name string
}

fn main() {
	println(json2.encode(Child{ name: "x" }))
	println(json2.decode[Child](\'{"id":2,"name":"y"}\') or { Child{} })
}
'

fn generated_c(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'json2_struct_codegen_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, '${name}.v')
	out := os.join_path(dir, '${name}.c')
	os.write_file(src, source) or { panic(err) }
	res := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(out)} ${os.quoted_path(src)}')
	assert res.exit_code == 0, res.output
	return os.read_file(out) or { panic(err) }
}

fn test_struct_without_embeds_does_not_specialize_embed_decoding() {
	c := generated_c('plain', plain_struct_program)
	// The key by key decoding through embedded structs is not generated at all...
	assert !c.contains('decode_struct_key')
	assert !c.contains('check_required_struct_fields_T')
	// ...and the fields are decoded by helpers specialized per field type.
	assert c.contains('decode_struct_field')
}

fn test_struct_with_embeds_uses_embed_decoding() {
	c := generated_c('embedding', embedding_struct_program)
	assert c.contains('decode_struct_key')
}
