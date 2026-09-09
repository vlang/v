// Regression test: an option pointer struct field must be copied (not unwrapped)
// when an `&Struct{...}` value containing it is converted to an interface.
// Previously `inside_interface_cast` leaked into the struct field value codegen,
// emitting `field = *(T**)val.data` for an option field, producing invalid C
// (`assigning to _option_..._ptr from incompatible type T*`).
// This broke building vlang/v-analyzer (analyzer/psi/element_factory.v).

interface Element {
	id() int
	file_name() string
}

@[heap]
struct File {
	name string
}

@[heap]
struct Node {
	id_val          int
	containing_file ?&File
}

fn (n &Node) id() int {
	return n.id_val
}

fn (n &Node) file_name() string {
	f := n.containing_file or { return '<none>' }
	return f.name
}

fn make_node(id_val int, containing_file ?&File) Element {
	return &Node{
		id_val:          id_val
		containing_file: containing_file
	}
}

fn test_option_field_preserved_through_interface_cast() {
	f := &File{
		name: 'main.v'
	}
	e := make_node(7, f)
	assert e.id() == 7
	assert e.file_name() == 'main.v'
}

fn test_none_option_field_through_interface_cast() {
	e := make_node(9, none)
	assert e.id() == 9
	assert e.file_name() == '<none>'
}
