module main

// The AST a .proto file parses into. It is a faithful-ish model of proto3 --
// message, nested message, enum, oneof, map, service, rpc -- and deliberately
// not a model of proto2: groups, extend, required, and default values are not
// represented, because the generator does not support them.

// Label is a field's cardinality.
pub enum Label {
	// singular is proto3's default: at most one value, and the default is not
	// written.
	singular
	// optional is a singular field with explicit presence.
	optional
	// repeated is a list, packed when the element type allows it.
	repeated
}

// FieldKind says what a field holds, which decides how it is written.
pub enum FieldKind {
	scalar
	bytes
	text
	message
	enum
	map
}

// Field is one field of a message.
pub struct Field {
pub mut:
	label Label
	kind  FieldKind
	// number is the protobuf field number, 1..2^29-1.
	number int
	// name is the field's name as the schema spells it.
	name string
	// type_name is the declared type, as the schema spells it: a scalar keyword
	// or a possibly-qualified message or enum name. Resolution happens later,
	// because a message can reference a type declared further down the file.
	type_name string
	// key_type and value_type are set only for a map field, and value_type is
	// empty for a `map<K, google.protobuf.Any>`-style Any value, which the
	// generator rejects.
	key_type   string
	value_type string
	// oneof is the name of the `oneof` this field belongs to, or empty.
	oneof string
	// packed_option is what `[packed = ...]` asked for, or an empty string when
	// the schema said nothing. The spec's default for a repeated numeric field is
	// packed, so an absent option is not the same as `false`.
	packed_option string
	// comments is the doc comment attached above the field, without its `//`.
	comments []string
	pos      Pos
}

// Message is a message declaration, including nested messages and enums.
pub struct Message {
pub mut:
	name     string
	fields   []Field
	messages []Message
	enums    []EnumDecl
	oneofs   []Oneof
	// reserved_ranges and reserved_names are recorded so the generator can
	// report a field that reuses one. A single reserved number is a range of
	// one; `9 to max` is kept as a range rather than expanded, since expanding
	// it would mean half a billion entries.
	reserved_ranges []ReservedRange
	reserved_names  []string
	comments        []string
	pos             Pos
}

// ReservedRange is an inclusive range of reserved field numbers.
pub struct ReservedRange {
pub:
	start int
	end   int
}

// contains reports whether `number` falls inside the range.
pub fn (r ReservedRange) contains(number int) bool {
	return number >= r.start && number <= r.end
}

// EnumDecl is an enum declaration, nested or top level.
pub struct EnumDecl {
pub mut:
	name   string
	values []EnumValue
	// allow_alias is `option allow_alias = true;`, which is what lets two
	// values share a number. Without it a shared number is a schema error.
	allow_alias bool
	comments    []string
	pos         Pos
}

// EnumValue is one enum member.
pub struct EnumValue {
pub mut:
	name     string
	number   int
	comments []string
}

// Oneof is a `oneof` group. The generator flattens it into its members: V
// sumtype variants carry no attributes in this compiler, so parallel optional
// fields sharing a group name are the representation it reads.
pub struct Oneof {
pub mut:
	name     string
	comments []string
}

// Rpc is one method of a service.
pub struct Rpc {
pub mut:
	name          string
	request_type  string
	response_type string
	// request_v_type and response_v_type are the resolved V types of the two
	// message names. They are filled in by the resolver rather than derived at
	// emit time, because the name an rpc writes can be qualified by its package
	// or ambiguous against another declaration, and only the resolver knows
	// which.
	request_v_type  string
	response_v_type string
	client_stream   bool
	server_stream   bool
	comments        []string
	pos             Pos
}

// Service is a service declaration.
pub struct Service {
pub mut:
	name     string
	rpcs     []Rpc
	comments []string
	pos      Pos
}

// Import records an `import` statement.
pub struct Import {
pub mut:
	// path is the quoted path as written, e.g. "google/protobuf/timestamp.proto".
	path string
	// public is true for `import public`, which re-exports the file's symbols.
	public bool
	// weak is true for `import weak`.
	weak bool
}

// File is a parsed .proto file.
pub struct File {
pub mut:
	path string
	// syntax is the declared syntax, `proto3` for everything supported here.
	syntax string
	// package is the declared package, possibly dotted, or empty.
	package string
	// package_parts is `package` split on dots, which is what the resolver and
	// the emitter both want.
	package_parts []string
	imports       []Import
	messages      []Message
	enums         []EnumDecl
	services      []Service
	// comments is the file's leading doc comment.
	comments []string
}

// full_name returns the file's fully qualified proto name, e.g. `google.rpc`.
pub fn (f &File) full_name() string {
	return if f.package == '' { f.proto_name() } else { '${f.package}.${f.proto_name()}' }
}

// proto_name returns the file's name without the directory and without the
// `.proto` extension.
pub fn (f &File) proto_name() string {
	mut base := f.path.all_after_last('/')
	base = base.all_after_last('\\')
	return base.all_before_last('.')
}
