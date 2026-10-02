module main

import encoding.protobuf

// The resolver turns a parsed File into something the emitter can write
// without making decisions: every field's declared type resolved to a V type,
// every message and enum given its final V name, and every reference mapped to
// the declaration it names.
//
// # Why resolution is separate from parsing
//
// A proto field may name a type declared anywhere -- later in the same file, in
// a nested message, in an imported file -- so a name is meaningless until the
// whole set of files is known. Resolving after parsing is what makes that
// possible, and it is also where an unresolvable name becomes one clear
// diagnostic instead of a wrong V type.

// Resolved is a field whose type is known and whose V type has been chosen.
pub struct Resolved {
pub mut:
	// v_type is the field's V type as declared, including a `[]` for a repeated
	// field and the `map[K]V` brackets for a map.
	v_type string
	// elem_type is the element's V type for a repeated field, and the same as
	// `v_type` otherwise. The emitter needs it to read and write one element.
	elem_type string
	// scalar is the ProtoScalar of a scalar field. For a repeated numeric field
	// it is the element's scalar, since that is what decides the encoding. It is
	// meaningless for a field that is not numeric.
	scalar protobuf.ProtoScalar
	// kind is refined from the parser's guess: a name that resolves to an enum
	// becomes `.enum` rather than `.message`.
	kind FieldKind
	// number is the protobuf field number, which the emitter writes literally
	// into the generated calls.
	number int
	// proto_type is the type name as the schema spells it, kept for the doc
	// comment on the generated field.
	proto_type string
	label      Label
	// oneof is the group this field belongs to, or empty. A generated field in a
	// group is declared optional, since a `oneof` is represented as parallel
	// optional fields sharing a group name.
	oneof string
	// name is the V field name, with a keyword collision suffixed.
	name string
	// comments is the doc comment the schema attached above the field.
	comments []string
	// map_key_type, map_value_kind, and map_value_type describe a map field's
	// value, which is written into the Entry message exactly as a field of that
	// type would be. They are empty for every other kind.
	map_key_type   string
	map_value_kind FieldKind
	map_value_type string
}

// is_repeated reports whether the field is a list.
pub fn (r &Resolved) is_repeated() bool {
	return r.label == .repeated
}

// is_packed reports whether a repeated field uses the packed encoding, which the
// spec makes the default for every repeated numeric type and which no
// length-delimited element type has.
pub fn (r &Resolved) is_packed() bool {
	if r.label != .repeated {
		return false
	}
	return r.kind == .scalar || r.kind == .enum
}

// ResolvedEnum is an enum with its final V name.
pub struct ResolvedEnum {
pub mut:
	v_name     string
	proto_name string
	values     []EnumValue
	comments   []string
}

// ResolvedMessage is a message with its final V name and resolved fields.
pub struct ResolvedMessage {
pub mut:
	v_name     string
	proto_name string
	// qualified is the message's fully qualified proto name, e.g.
	// `kv.Outer.Inner`, which is what a field referring to it writes.
	qualified string
	// doc_name is the same name in the form the generated doc comments read
	// best: `kv.Outer.Inner`. It is kept apart from `qualified` because a
	// nested message's declaration chain carries no package of its own, so
	// building the display form here is what stops the doc comment from saying
	// `demo..Inner`.
	doc_name string
	fields   []Resolved
	comments []string
}

// ResolvedFile is a whole schema, ready to emit.
pub struct ResolvedFile {
pub mut:
	// module is the V module name the generated file should declare.
	module string
	// package is the proto package, for the path constants and diagnostics.
	package  string
	messages []ResolvedMessage
	enums    []ResolvedEnum
	services []Service
	// errors is every problem found while resolving. A non-empty list means the
	// generator refuses to emit: a codec for a schema it did not fully
	// understand would silently disagree with the producer.
	errors []string
}

// TypeIndex holds every type name declared across the parsed files, so a
// reference can be resolved.
pub struct TypeIndex {
pub mut:
	// enums holds the fully qualified proto names of every enum.
	enums map[string]bool
	// messages holds the fully qualified proto names of every message. Both
	// maps are needed: an unresolved name has to be reported as unknown rather
	// than silently treated as a message.
	messages map[string]bool
	// claimed counts how many distinct qualified names claim each plain V
	// name. A name claimed once keeps it; a name claimed twice is ambiguous and
	// has to be qualified, or two declarations would collide.
	claimed map[string]int
}

// new_index returns an empty index.
pub fn new_index() TypeIndex {
	return TypeIndex{
		enums:    map[string]bool{}
		messages: map[string]bool{}
		claimed:  map[string]int{}
	}
}

// claim records that `qualified` wants the V name `plain`.
fn (mut idx TypeIndex) claim(qualified string, plain string) {
	if idx.messages[qualified] || idx.enums[qualified] {
		return
	}
	idx.claimed[plain]++
}

// v_name_of returns the V name for a fully qualified proto name.
//
// A name unique across the schema keeps its plain PascalCase form, because that
// is what a reader expects to see: a message called `GetRequest` in package `kv`
// becomes `GetRequest`, not `KvGetRequest`. Only a name two declarations both
// want is qualified, which is the case that would otherwise produce two
// identical V structs.
pub fn (idx &TypeIndex) v_name_of(qualified string) string {
	plain := safe_type_name(target_name(qualified))
	if idx.claimed[plain] <= 1 {
		return plain
	}
	return flatten_name(package_of(qualified), target_name(qualified))
}

// resolve_files resolves `files` as one schema, and returns the result to emit.
//
// `module_name` is the V module name the generated file should declare. An empty
// string means derive it from the first file's package.
pub fn resolve_files(files []File, module_name string) !ResolvedFile {
	mut res := ResolvedFile{
		module: module_name
	}
	mut idx := new_index()
	// Everything is indexed before anything is resolved, so a field can refer to
	// a type declared further down its own file.
	for f in files {
		collect_index(mut idx, f)
	}
	// Then every claim on a plain name is counted, so a name two declarations
	// both want is qualified and one nobody else wants is not.
	for f in files {
		claim_index(mut idx, f)
	}
	for f in files {
		resolve_file(mut res, &idx, f)
	}
	if res.module == '' && files.len > 0 {
		res.module = default_module_name(files[0])
	}
	// The package is what a gRPC path is built from, and it is a property of the
	// schema rather than of the V module name: `google.rpc.Status` gives the
	// path `/google.rpc.Status/Method`, whatever the module is called.
	if files.len > 0 {
		res.package = files[0].package
	}
	// Messages are emitted in name order rather than declaration order, so two
	// runs over the same schema produce byte-identical output and a diff of the
	// generated file shows only what the schema changed.
	res.messages.sort_with_compare(fn [ResolvedMessage](a &ResolvedMessage, b &ResolvedMessage) int {
		return cmp_strings(a.v_name, b.v_name)
	})
	return res
}

// cmp_strings orders two strings, which `compare` is the builtin for but which
// is spelled out here so the sort's intent is obvious at the call site.
fn cmp_strings(a string, b string) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// default_module_name derives a V module name from a schema's package, e.g.
// `google.rpc` becomes `google_rpc`, which is also the directory layout V's
// import rules expect.
pub fn default_module_name(f File) string {
	if f.package == '' {
		return snake_case(f.proto_name())
	}
	mut out := []u8{}
	for i, part in f.package_parts {
		if i > 0 {
			out << `_`
		}
		out << snake_case(part).bytes()
	}
	return out.bytestr()
}

// collect_index records every message and enum name the file declares, prefixed
// by the package it is in.
fn collect_index(mut idx TypeIndex, f File) {
	prefix := if f.package == '' { '' } else { '${f.package}.' }
	for e in f.enums {
		idx.enums[prefix + e.name] = true
	}
	for m in f.messages {
		collect_message_index(mut idx, prefix, m)
	}
}

// collect_message_index records a message and, recursively, its nested ones.
fn collect_message_index(mut idx TypeIndex, prefix string, m Message) {
	qualified := prefix + m.name
	idx.messages[qualified] = true
	for e in m.enums {
		idx.enums[qualified + '.' + e.name] = true
	}
	for nested in m.messages {
		collect_message_index(mut idx, qualified + '.', nested)
	}
}

// claim_index counts how many declarations want each plain V name, so an
// ambiguous one can be qualified.
fn claim_index(mut idx TypeIndex, f File) {
	prefix := if f.package == '' { '' } else { '${f.package}.' }
	for e in f.enums {
		idx.claim(prefix + e.name, safe_type_name(e.name))
	}
	for m in f.messages {
		claim_message_index(mut idx, prefix, m)
	}
}

// claim_message_index counts the claims a message and its nested declarations
// make, recursively.
fn claim_message_index(mut idx TypeIndex, prefix string, m Message) {
	qualified := prefix + m.name
	idx.claim(qualified, safe_type_name(m.name))
	for e in m.enums {
		idx.claim(qualified + '.' + e.name, safe_type_name(e.name))
	}
	for nested in m.messages {
		claim_message_index(mut idx, qualified + '.', nested)
	}
}

// resolve_file resolves one file's declarations into `res`.
fn resolve_file(mut res ResolvedFile, idx &TypeIndex, f File) {
	// The package is passed without a trailing dot: `resolve_message` joins the
	// parent and the name with one, and a trailing dot produced the qualified
	// name `demo..Inner`.
	for e in f.enums {
		qualified := if f.package == '' { e.name } else { '${f.package}.${e.name}' }
		res.enums << resolve_enum(idx, qualified, e)
	}
	for m in f.messages {
		resolve_message(mut res, idx, f.package, m)
	}
	for s in f.services {
		res.services << resolve_service(mut res, idx, f.package, s)
	}
}

// resolve_service resolves an rpc's request and response message names.
//
// This has to happen here rather than at emit time because the emitter cannot
// know whether a name needs qualifying: only the index knows whether some other
// declaration already claims that V name.
fn resolve_service(mut res ResolvedFile, idx &TypeIndex, package string, s Service) Service {
	mut out := s
	for i, r in s.rpcs {
		req := resolve_type_name(idx, package, r.request_type) or {
			res.errors << 'pbgen: ${s.name}.${r.name} refers to unknown request type `${r.request_type}`'
			continue
		}
		resp := resolve_type_name(idx, package, r.response_type) or {
			res.errors << 'pbgen: ${s.name}.${r.name} refers to unknown response type `${r.response_type}`'
			continue
		}
		if !idx.messages[req] {
			res.errors << 'pbgen: ${s.name}.${r.name} request `${r.request_type}` is not a message'
			continue
		}
		if !idx.messages[resp] {
			res.errors << 'pbgen: ${s.name}.${r.name} response `${r.response_type}` is not a message'
			continue
		}
		out.rpcs[i].request_v_type = idx.v_name_of(req)
		out.rpcs[i].response_v_type = idx.v_name_of(resp)
	}
	return out
}

// resolve_enum gives an enum its V name and copies its values.
fn resolve_enum(idx &TypeIndex, qualified string, e EnumDecl) ResolvedEnum {
	mut out := ResolvedEnum{
		v_name:     idx.v_name_of(qualified)
		proto_name: e.name
		values:     e.values
		comments:   e.comments
	}
	// An enum value is a V enum value name, so a proto value that collides with
	// a V keyword has to be suffixed. proto3 already reserves a `_UNSPECIFIED`
	// style name for the zero value, which is not a collision, but a schema is
	// free to name it `type`.
	for i, v in out.values {
		if is_v_keyword(v.name) {
			out.values[i].name = '${v.name}_'
		}
	}
	return out
}

// resolve_message resolves a message and, recursively, its nested ones.
fn resolve_message(mut res ResolvedFile, idx &TypeIndex, parent_qualified string, m Message) {
	// A nested message's V name flattens its whole chain when it has to, since V
	// has no nested types: `Outer.Inner` becomes `OuterInner`.
	qualified := if parent_qualified == '' {
		m.name
	} else {
		'${parent_qualified}.${m.name}'
	}
	mut out := ResolvedMessage{
		v_name:     idx.v_name_of(qualified)
		proto_name: m.name
		qualified:  qualified
		comments:   m.comments
	}
	// A reserved field number or name is a schema error the generator can see,
	// so it is reported rather than emitted: protoc refuses these too.
	reserved := m.reserved_numbers
	reserved_names := m.reserved_names
	for f in m.fields {
		if f.number in reserved {
			res.errors << 'pbgen: ${qualified}.${f.name} uses field number ${f.number}, which the schema reserves'
		}
		if f.name in reserved_names {
			res.errors << 'pbgen: ${qualified}.${f.name} uses a name the schema reserves'
		}
		r := resolve_field(mut res, idx, qualified, f)
		if r.v_type == '' {
			continue
		}
		out.fields << r
	}
	// The declared name is `Outer.Inner`, with no package in front of it: the
	// parent chain already says everything about where it sits.
	out.proto_name = m.name
	// The doc comment wants the full proto path, package included, so it is
	// assembled here rather than reusing the chain above.
	mut full := m.name
	if parent_qualified != '' {
		full = parent_qualified + '.' + m.name
	}
	if res.package != '' {
		full = res.package + '.' + full
	}
	out.doc_name = full
	res.messages << out
	for nested in m.messages {
		resolve_message(mut res, idx, qualified, nested)
	}
	for e in m.enums {
		res.enums << resolve_enum(idx, qualified + '.' + e.name, e)
	}
}

// flatten_name flattens a dotted proto name into a PascalCase V name, so
// `pkg.Outer.Inner` becomes `PkgOuterInner`.
pub fn flatten_name(qualified string, name string) string {
	mut out := []u8{}
	if qualified != '' {
		for part in qualified.split('.') {
			out << pascal_case(part).bytes()
		}
	}
	out << safe_type_name(name).bytes()
	return out.bytestr()
}

// resolve_field resolves one field's declared type to a V type.
fn resolve_field(mut res ResolvedFile, idx &TypeIndex, parent_qualified string, f Field) Resolved {
	mut out := Resolved{
		name:       safe_field_name(f.name)
		kind:       f.kind
		number:     f.number
		proto_type: f.type_name
		label:      f.label
		oneof:      f.oneof
		comments:   f.comments
	}
	if f.kind == .map {
		return resolve_map(mut res, idx, parent_qualified, f)
	}
	mut elem := ''
	match f.kind {
		.scalar {
			scalar := protobuf.scalar_by_name(f.type_name) or {
				res.errors << 'pbgen: ${parent_qualified}.${f.name} has unknown scalar type `${f.type_name}`'
				return Resolved{}
			}
			out.scalar = scalar
			elem = v_type_for_scalar(scalar)
		}
		.text {
			elem = 'string'
		}
		.bytes {
			elem = '[]u8'
		}
		.message {
			target := resolve_type_name(idx, parent_qualified, f.type_name) or {
				res.errors <<
					'pbgen: ${parent_qualified}.${f.name} refers to unknown type `${f.type_name}`'
				return Resolved{}
			}
			// An enum travels as its integer value, so its V field type is the
			// enum itself and its wire type is a varint. A message and an enum
			// resolve to the same V name here, because the kind above is what
			// tells them apart.
			if idx.enums[target] {
				out.kind = .enum
				out.scalar = .int32
			}
			elem = idx.v_name_of(target)
		}
		else {}
	}
	out.elem_type = elem
	// A repeated field becomes a list. A singular `bytes` is already a slice,
	// so only a list of them is wrapped -- a `repeated bytes` is `[][]u8`, not
	// `[]u8`.
	if f.label == .repeated && !(f.kind == .bytes && f.label == .singular) {
		out.v_type = '[]${elem}'
	} else {
		out.v_type = elem
	}
	return out
}

// resolve_map resolves a `map<K, V>` field. The wire form is a repeated Entry
// message with key = 1 and value = 2, and the V form is a `map[K]V`.
fn resolve_map(mut res ResolvedFile, idx &TypeIndex, parent_qualified string, f Field) Resolved {
	mut out := Resolved{
		kind:       .map
		name:       safe_field_name(f.name)
		number:     f.number
		proto_type: f.type_name
		label:      .repeated
		comments:   f.comments
	}
	key_type := v_type_for_map_key(f.key_type)
	if key_type == '' {
		res.errors << 'pbgen: ${parent_qualified}.${f.name} has map key type `${f.key_type}`, which is not a protobuf map key'
		return Resolved{}
	}
	mut value_type := ''
	mut value_kind := classify_type(f.value_type)
	mut value_scalar := protobuf.ProtoScalar.int32
	match value_kind {
		.scalar {
			scalar := protobuf.scalar_by_name(f.value_type) or {
				res.errors << 'pbgen: ${parent_qualified}.${f.name} has unknown map value type `${f.value_type}`'
				return Resolved{}
			}
			value_scalar = scalar
			value_type = v_type_for_scalar(scalar)
		}
		.text {
			value_type = 'string'
		}
		.bytes {
			value_type = '[]u8'
		}
		else {
			target := resolve_type_name(idx, parent_qualified, f.value_type) or {
				res.errors <<
					'pbgen: ${parent_qualified}.${f.name} refers to unknown map value type `${f.value_type}`'
				return Resolved{}
			}
			if idx.enums[target] {
				value_kind = .enum
			}
			value_type = idx.v_name_of(target)
		}
	}
	// The emitter needs the value's shape, since a map value is written into the
	// Entry message exactly as a field of that type would be.
	out.elem_type = value_type
	out.scalar = value_scalar
	// map_key_type and map_value_kind are read back by the emitter; they are
	// carried as a small struct so the V type string stays the single source of
	// truth for the declaration.
	out.map_key_type = key_type
	out.map_value_kind = value_kind
	out.map_value_type = value_type
	out.v_type = 'map[${key_type}]${value_type}'
	return out
}

// v_type_for_scalar returns the V type a ProtoScalar is carried in.
//
// The mapping is deliberately not always the obvious one. An `i32` field
// declared `int32` and the same field declared `sint32` share a V type but not
// a wire encoding, so the V type alone cannot say which -- which is exactly why
// the emitted code carries an explicit writer per scalar rather than relying on
// the field's type.
pub fn v_type_for_scalar(s protobuf.ProtoScalar) string {
	return match s {
		.boolean { 'bool' }
		.int32, .sint32, .sfixed32 { 'i32' }
		.int64, .sint64, .sfixed64 { 'i64' }
		.uint32, .fixed32 { 'u32' }
		.uint64, .fixed64 { 'u64' }
		.float32 { 'f32' }
		.float64 { 'f64' }
	}
}

// v_type_for_map_key returns the V type for a map key, or an empty string when
// the key type is not one protobuf allows. A map key may be any integral,
// bool, or string type; the floating point types are not allowed, because a
// float key would not survive a round trip reliably.
pub fn v_type_for_map_key(name string) string {
	return match name {
		'string' { 'string' }
		'bool' { 'bool' }
		'int32', 'sint32', 'sfixed32' { 'i32' }
		'int64', 'sint64', 'sfixed64' { 'i64' }
		'uint32', 'fixed32' { 'u32' }
		'uint64', 'fixed64' { 'u64' }
		else { '' }
	}
}

// resolve_type_name turns a possibly-relative type name into a fully qualified
// proto name.
//
// proto resolves a bare name by walking outwards: the current message, then its
// enclosing message, then the package. That order is what makes
// `Inner` inside `Outer` mean `Outer.Inner` rather than a top-level `Inner`.
pub fn resolve_type_name(idx &TypeIndex, parent_qualified string, name string) ?string {
	if name.contains('.') {
		// A leading dot marks an already-absolute name.
		if name.starts_with('.') {
			candidate := name[1..]
			if idx.messages[candidate] || idx.enums[candidate] {
				return candidate
			}
			return none
		}
		// A dotted name is resolved against the innermost scope that contains
		// it, so `google.rpc.Status` is found from inside any message.
		mut parts := parent_qualified.split('.')
		// Drop the message itself: a field's type name is resolved from the
		// scope containing the message, not from the message.
		if parts.len > 0 && idx.messages[parent_qualified] {
			parts = parts[..parts.len - 1]
		}
		for i := parts.len; i >= 0; i-- {
			prefix := if i == 0 { '' } else { parts[..i].join('.') + '.' }
			candidate := prefix + name
			if idx.messages[candidate] || idx.enums[candidate] {
				return candidate
			}
		}
		return none
	}
	// A bare name walks outwards one scope at a time.
	mut scope := parent_qualified
	for {
		candidate := if scope == '' { name } else { scope + '.' + name }
		if idx.messages[candidate] || idx.enums[candidate] {
			return candidate
		}
		parts := scope.split('.')
		if parts.len == 0 || parts.len == 1 {
			break
		}
		scope = parts[..parts.len - 1].join('.')
	}
	if idx.messages[name] || idx.enums[name] {
		return name
	}
	return none
}

// package_of returns the package part of a fully qualified name, which is
// everything before its last dot.
pub fn package_of(qualified string) string {
	idx := qualified.last_index('.') or { return '' }
	return qualified[..idx]
}

// target_name returns the last component of a fully qualified name.
pub fn target_name(qualified string) string {
	return qualified.all_after_last('.')
}
