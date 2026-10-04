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
	// oneof_members lists every field of `oneof`, this one included. A reader
	// needs it because the members are parallel optionals, and only the reader
	// can enforce that exactly one of them is set.
	oneof_members []string
	// name is the V field name: the schema's name in snake_case, with a keyword
	// collision suffixed.
	name string
	// proto_name is the field's name as the schema spells it, for the doc comment
	// and for diagnostics.
	proto_name string
	// comments is the doc comment the schema attached above the field.
	comments []string
	// map_key_type, map_value_kind, and map_value_type describe a map field's
	// key and value, which are written into the Entry message exactly as a field
	// of that type would be. They are empty for every other kind.
	map_key_type   string
	map_value_kind FieldKind
	map_value_type string
	// map_key_kind is `.text` for a string key and `.scalar` for every other one,
	// and map_key_scalar is the key's ProtoScalar when it is a scalar. The scalar
	// comes from the schema's key type and not from the V type, because `i32`
	// stands for `int32`, `sint32`, and `sfixed32`, which are three encodings.
	map_key_kind   FieldKind
	map_key_scalar protobuf.ProtoScalar
	// indirect marks a singular message field whose type contains, directly or
	// through other singular message fields, the message the field is in. V
	// accepts such a recursive struct only through an optional pointer, so the
	// field is declared `?&T` rather than `?T`.
	indirect bool
	// packed_option is the raw `[packed = ...]` value, empty when the schema said
	// nothing. `is_packed` reads it rather than a resolved bool, because the
	// three-way answer (absent, true, false) is what decides the default.
	packed_option string
}

// is_repeated reports whether the field is a list.
pub fn (r &Resolved) is_repeated() bool {
	return r.label == .repeated
}

// has_explicit_presence reports whether the field records whether it was set,
// rather than only what it holds.
//
// Three things in the schema mean yes. A proto3 `optional` field does: the spec
// models it as a synthetic one-member `oneof`, so `false` and `""` are
// distinguishable from never having been sent. A member of a real `oneof` does,
// because the group's whole purpose is to record which member was chosen. And a
// singular message field always does: an empty nested message is still a
// field on the wire, so a peer can tell one that was set to `{}` from one that
// was never set.
//
// None of these is true of a plain singular scalar, where an absent field and a
// field holding its default are the same thing on the wire, nor of a repeated
// field or a map, where an empty list is an absent one.
pub fn (r &Resolved) has_explicit_presence() bool {
	if r.label == .repeated {
		return false
	}
	return r.label == .optional || r.oneof != '' || r.kind == .message
}

// is_packed reports whether a repeated field uses the packed encoding, which the
// spec makes the default for every repeated numeric type and which no
// length-delimited element type has.
//
// `[packed = false]` turns it off for a numeric field. Both forms are legal on
// the wire, so a reader has to accept either, which is why the decoder already
// does; honouring the option only keeps the bytes matching what the schema asked
// for.
pub fn (r &Resolved) is_packed() bool {
	if r.label != .repeated {
		return false
	}
	if r.kind != .scalar && r.kind != .enum {
		return false
	}
	return r.packed_option != 'false'
}

// ResolvedEnum is an enum with its final V name.
pub struct ResolvedEnum {
pub mut:
	v_name     string
	proto_name string
	// values carry their V names: snake_case, with a keyword suffixed.
	values []EnumValue
	// allow_alias is set when two values share a number, which the schema has
	// to permit with `option allow_alias = true` and V with an attribute.
	allow_alias bool
	comments    []string
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
	// counted is the set of qualified names already counted in `claimed`. It is
	// separate from `messages` and `enums` because those are filled in before
	// any claim is counted: testing membership there made every claim return
	// early, so nothing was ever counted and no name was ever qualified.
	counted map[string]bool
}

// new_index returns an empty index.
pub fn new_index() TypeIndex {
	return TypeIndex{
		enums:    map[string]bool{}
		messages: map[string]bool{}
		claimed:  map[string]int{}
		counted:  map[string]bool{}
	}
}

// claim records that `qualified` wants the V name `plain`.
//
// A qualified name is counted once, so the same declaration reaching this twice
// (an import reachable by more than one path) does not make a unique name look
// ambiguous.
fn (mut idx TypeIndex) claim(qualified string, plain string) {
	if idx.counted[qualified] {
		return
	}
	idx.counted[qualified] = true
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
	check_type_names(mut res)
	mark_recursive_fields(mut res)
	check_function_names(mut res)
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
		res.enums << resolve_enum(mut res, idx, qualified, e)
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

// resolve_enum gives an enum its V name and its values their V names.
fn resolve_enum(mut res ResolvedFile, idx &TypeIndex, qualified string, e EnumDecl) ResolvedEnum {
	mut out := ResolvedEnum{
		v_name:     idx.v_name_of(qualified)
		proto_name: e.name
		values:     e.values.clone()
		comments:   e.comments
	}
	if e.values.len == 0 {
		res.errors << 'pbgen: enum `${qualified}` has no values, and proto3 requires at least the zero value'
		return out
	}
	// proto3 makes the first value the default, and the default of an enum is
	// zero: a field that was never sent decodes to it.
	if e.values[0].number != 0 {
		res.errors << 'pbgen: enum `${qualified}` starts with `${e.values[0].name} = ${e.values[0].number}`, and proto3 requires the first value to be zero'
	}
	// An enum value is a V enum field, and V refuses an uppercase letter there,
	// so the conventional `COLOR_RED` becomes `color_red`. A value that then
	// collides with a V keyword is suffixed: a schema is free to name one `type`.
	mut by_name := map[string]string{}
	mut by_number := map[int]string{}
	for i, v in e.values {
		name := safe_field_name(snake_case(v.name))
		if name in by_name {
			res.errors << 'pbgen: enum `${qualified}` values `${by_name[name]}` and `${v.name}` would both be declared as `${name}`'
		}
		by_name[name] = v.name
		out.values[i].name = name
		// Two values on one number is an alias. protoc refuses it unless the
		// schema allows it, and V refuses it unless the enum is marked.
		if v.number in by_number {
			if !e.allow_alias {
				res.errors << 'pbgen: enum `${qualified}` values `${by_number[v.number]}` and `${v.name}` share the number ${v.number}, which needs `option allow_alias = true;`'
			}
			out.allow_alias = true
		} else {
			by_number[v.number] = v.name
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
	reserved_names := m.reserved_names
	for f in m.fields {
		for r in m.reserved_ranges {
			if r.contains(f.number) {
				res.errors << 'pbgen: ${qualified}.${f.name} uses field number ${f.number}, which the schema reserves'
				break
			}
		}
		if f.name in reserved_names {
			res.errors << 'pbgen: ${qualified}.${f.name} uses a name the schema reserves'
		}
		r := resolve_field(mut res, idx, qualified, f)
		if r.v_type == '' {
			continue
		}
		check_packed_option(mut res, qualified, r)
		out.fields << r
	}
	check_field_declarations(mut res, qualified, out.fields)
	// Every member of a `oneof` learns the whole membership of its group, because
	// the exclusivity is the reader's job and only a member knows the others.
	for i, f in out.fields {
		if f.oneof == '' {
			continue
		}
		out.fields[i].oneof_members = members_of(out.fields, f.oneof)
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
		res.enums << resolve_enum(mut res, idx, qualified + '.' + e.name, e)
	}
}

// check_packed_option reports a `packed = true` on a field that cannot be packed.
//
// Only numeric and enum elements have a packed form; a string, a bytes field, and
// a message are length-delimited already. protoc refuses the combination rather
// than ignoring it, and silently accepting it here would mean the schema said one
// thing and the codec did another.
//
// `packed = false` is not an error on any field: on a non-numeric field it is
// redundant, and the schema is allowed to be explicit about something the default
// already does.
fn check_packed_option(mut res ResolvedFile, qualified string, f Resolved) {
	if f.packed_option != 'true' {
		return
	}
	if f.label == .repeated && f.kind != .scalar && f.kind != .enum {
		res.errors << 'pbgen: ${qualified}.${f.proto_name} asks for the packed form, but a `${f.proto_type}` field has no packed form'
	}
	if f.label != .repeated && f.kind != .map {
		res.errors << 'pbgen: ${qualified}.${f.proto_name} asks for the packed form, but only a repeated field can be packed'
	}
}

// check_field_declarations reports the ways a message's field list can be
// invalid in ways that would otherwise reach the output as broken V.
//
// Each of these was silently accepted before: two fields on one number emit two
// identical `match` arms, a name repeated emits two struct fields with one name,
// and a number outside the legal range either cannot be written or overflows the
// three bits the wire format keeps for the wire type. protoc refuses all of them,
// and a codec that quietly disagreed with the producer would be worse than a
// refusal, so they are reported here.
fn check_field_declarations(mut res ResolvedFile, qualified string, fields []Resolved) {
	mut by_number := map[int]string{}
	mut by_name := map[string]string{}
	for f in fields {
		if f.number < protobuf.min_field_number || f.number > protobuf.max_field_number {
			res.errors << 'pbgen: ${qualified}.${f.proto_name} has field number ${f.number}, which is outside the legal range ${protobuf.min_field_number} to ${protobuf.max_field_number}'
			continue
		}
		if f.number in by_number {
			res.errors << 'pbgen: ${qualified}.${f.proto_name} uses field number ${f.number}, which field `${by_number[f.number]}` already uses'
			continue
		}
		by_number[f.number] = f.proto_name
		// The V name has to be unique, since that is what the struct declares, and
		// `f.name` is already the V name: the resolver applies the snake_case and
		// the keyword suffix that can make two proto names collide, as `userName`
		// and `user_name` do.
		if f.name in by_name {
			res.errors << 'pbgen: ${qualified}.${f.proto_name} collides with field `${by_name[f.name]}` of the same message, so both would be declared as `${f.name}`'
			continue
		}
		by_name[f.name] = f.proto_name
	}
}

// check_type_names reports declared types whose V name V will not accept.
//
// A one-letter capital name is reserved for generic template types. V says so
// when the declaration is an enum, but accepts it for a struct and then lowers
// the type's uses to `int`, so the generated file compiles on its own and fails
// at the first call with a diagnostic about a C conversion.
//
// Two declarations that end up with one V name are reported too. The index
// qualifies a name two declarations want, but `get_request` and `GetRequest` in
// one package still flatten to the same qualified name.
fn check_type_names(mut res ResolvedFile) {
	mut seen := map[string]string{}
	for m in res.messages {
		if single_capital_name(m.v_name) {
			res.errors << 'pbgen: message `${m.proto_name}` becomes the type `${m.v_name}`, and a single letter capital name is reserved for generic template types. Rename it in the schema.'
		}
		if m.v_name in seen {
			res.errors << 'pbgen: message `${m.qualified}` and ${seen[m.v_name]} would both be declared as the type `${m.v_name}`'
		}
		seen[m.v_name] = 'message `${m.qualified}`'
	}
	for en in res.enums {
		if single_capital_name(en.v_name) {
			res.errors << 'pbgen: enum `${en.proto_name}` becomes the type `${en.v_name}`, and a single letter capital name is reserved for generic template types. Rename it in the schema.'
		}
		if en.v_name in seen {
			res.errors << 'pbgen: enum `${en.proto_name}` and ${seen[en.v_name]} would both be declared as the type `${en.v_name}`'
		}
		seen[en.v_name] = 'enum `${en.proto_name}`'
	}
}

// mark_recursive_fields marks every singular message field that makes its
// message recursive, so the emitter declares it `?&T`.
//
// A message may contain itself, as `message Node { Node child = 1; }` does, or
// reach itself through others. V refuses a struct that contains itself by value,
// even through an option, and accepts one only through an optional pointer. A
// repeated field and a map are not edges here: a V array or map holds its
// elements on the heap, so `[]Node` inside `Node` is fine as it is.
//
// Only the fields on a cycle become pointers. Every other message field keeps
// its value type, which is what a reader of the generated struct expects.
fn mark_recursive_fields(mut res ResolvedFile) {
	mut edges := map[string][]string{}
	for m in res.messages {
		mut targets := []string{}
		for f in m.fields {
			if f.kind == .message && f.label != .repeated {
				targets << f.elem_type
			}
		}
		edges[m.v_name] = targets
	}
	for mut m in res.messages {
		for mut f in m.fields {
			if f.kind == .message && f.label != .repeated {
				f.indirect = reaches(edges, f.elem_type, m.v_name)
			}
		}
	}
}

// reaches reports whether message `from` contains message `to`, itself included,
// following `edges`.
fn reaches(edges map[string][]string, from string, to string) bool {
	mut seen := map[string]bool{}
	mut stack := [from]
	for stack.len > 0 {
		cur := stack.pop()
		if cur == to {
			return true
		}
		if seen[cur] {
			continue
		}
		seen[cur] = true
		for next in edges[cur] {
			stack << next
		}
	}
	return false
}

// check_function_names reports two declarations that would generate the same
// function.
//
// The codec declares its functions at module level, named after the message
// and, for a map, the field. Names that are distinct in the schema can still
// meet there: message `FooWith` and the `_with` decoder of message `Foo` are
// both `decode_foo_with`. V reports that as a redefinition in a file nobody
// wrote, so it is reported here against the schema instead.
fn check_function_names(mut res ResolvedFile) {
	// owner maps a function name to the message that wants it, and owner_type to
	// that message's V type. Two messages of one V type are already reported by
	// check_type_names, so their functions are not reported a second time.
	mut owner := map[string]string{}
	mut owner_type := map[string]string{}
	mut reported := map[string]bool{}
	for m in res.messages {
		mut wanted := [decode_fn_name(m.v_name), decode_with_fn_name(m.v_name), read_fn_name(m.v_name)]
		for f in m.fields {
			if f.kind == .map {
				wanted << map_encode_fn_name(m.v_name, f.name)
				wanted << map_entry_read_fn_name(m.v_name, f.name)
			}
		}
		for name in wanted {
			if name in owner && owner[name] != m.qualified {
				pair := '${owner[name]} ${m.qualified}'
				if owner_type[name] != m.v_name && !reported[pair] {
					res.errors << 'pbgen: message `${m.qualified}` and message `${owner[name]}` would both generate the function `${name}`. Rename one of them in the schema.'
					reported[pair] = true
				}
				continue
			}
			owner[name] = m.qualified
			owner_type[name] = m.v_name
		}
	}
}

// members_of returns the names of every field in group `group`.
//
// It is keyed off the group name rather than being computed once, because the
// members are resolved one at a time and any of them may be the first resolved.
fn members_of(fields []Resolved, group string) []string {
	mut out := []string{}
	for f in fields {
		if f.oneof == group {
			out << f.name
		}
	}
	return out
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
		name:          safe_field_name(snake_case(f.name))
		proto_name:    f.name
		kind:          f.kind
		number:        f.number
		proto_type:    f.type_name
		label:         f.label
		oneof:         f.oneof
		// An empty `packed_option` means the schema said nothing, which is not the
		// same as `false`: the spec defaults a repeated numeric field to packed.
		packed_option: f.packed_option
		comments:      f.comments
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
		name:       safe_field_name(snake_case(f.name))
		proto_name: f.name
		number:     f.number
		proto_type: 'map<${f.key_type}, ${f.value_type}>'
		label:      .repeated
		comments:   f.comments
	}
	key_type := v_type_for_map_key(f.key_type)
	if key_type == '' {
		res.errors << 'pbgen: ${parent_qualified}.${f.name} has map key type `${f.key_type}`, which is not a protobuf map key'
		return Resolved{}
	}
	if f.key_type == 'string' {
		out.map_key_kind = .text
	} else {
		out.map_key_kind = .scalar
		// v_type_for_map_key accepted the name, so it is one of the integral
		// scalars or `bool`.
		out.map_key_scalar = protobuf.scalar_by_name(f.key_type) or { protobuf.ProtoScalar.int32 }
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
