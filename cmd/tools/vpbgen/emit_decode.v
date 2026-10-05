module main

import encoding.protobuf

// emit_message_decode writes the three functions that read a message: the
// public `decode_<message>(data)` and `decode_<message>_with(data, opts)`, and
// the private `read_<message>(mut unpacker, mut out)` both are built on.
//
// The reader takes an Unpacker rather than bytes, so a nested message is read
// through its parent's: `Unpacker.sub` carries the options and the nesting depth
// down, and `enter` is what lets `DecodeOpts.max_depth` stop a payload that
// nests deeper than the stack can follow. A reader that built a fresh Unpacker
// per message started every level at depth zero, so the bound never applied.
//
// It reads into an existing value rather than returning a new one, because a
// message field that occurs twice on the wire is merged rather than replaced:
// the spec says so, and a producer that splits a message relies on it.
pub fn emit_message_decode(mut e Emitter, m ResolvedMessage) {
	decode_name := decode_fn_name(m.v_name)
	with_name := decode_with_fn_name(m.v_name)
	read_name := read_fn_name(m.v_name)
	e.wln(0, '// ${decode_name} parses the proto3 message `${m.doc_name}`.')
	e.wln(0, '//')
	e.wln(0, '// A field this build does not know is skipped rather than rejected, because')
	e.wln(0, '// forward compatibility is a requirement of the format: a newer producer')
	e.wln(0, '// must not break an older consumer. A field that is known but arrives with')
	e.wln(0, '// the wrong wire type is an error, since that is a real disagreement')
	e.wln(0, '// rather than a field from the future.')
	e.wln(0, 'pub fn ${decode_name}(data []u8) !${m.v_name} {')
	e.wln(1, 'return ${with_name}(data, protobuf.DecodeOpts{})')
	e.wln(0, '}')
	e.w('')
	e.wln(0, '// ${with_name} parses the proto3 message `${m.doc_name}` with `opts`, which')
	e.wln(0, '// bound how deeply messages may nest and how long a field may be.')
	e.wln(0, 'pub fn ${with_name}(data []u8, opts protobuf.DecodeOpts) !${m.v_name} {')
	e.wln(1, 'mut out := ${m.v_name}{}')
	e.wln(1, 'mut unpacker := protobuf.new_unpacker(data, opts)')
	e.wln(1, '${read_name}(mut unpacker, mut out)!')
	e.wln(1, 'return out')
	e.wln(0, '}')
	e.w('')
	e.wln(0, '// ${read_name} reads the fields of `${m.doc_name}` from `unpacker` into `out`')
	e.wln(0, '// until the input ends. A scalar that arrives again replaces the earlier')
	e.wln(0, '// value, a list or a map gains the new elements, and a nested message is')
	e.wln(0, '// merged into the one already there.')
	e.wln(0, 'fn ${read_name}(mut unpacker protobuf.Unpacker, mut out ${m.v_name}) ! {')
	if m.fields.len == 0 {
		// Nothing is ever assigned, but every field still has to be stepped over:
		// an empty message is how a schema leaves room for fields added later.
		e.wln(1, '_ = out')
	}
	e.wln(1, 'for !unpacker.eof() {')
	e.wln(2, 'number, wire_type := unpacker.read_tag()!')
	if m.fields.len == 0 {
		e.wln(2, '// an unknown field, since this message declares none')
		e.wln(2, 'unpacker.skip_field(number, wire_type)!')
	} else {
		e.wln(2, 'match number {')
		for f in m.fields {
			emit_decode_case(mut e, m, f)
		}
		e.wln(3, 'else {')
		e.wln(4, '// an unknown field, or one this build does not know')
		e.wln(4, 'unpacker.skip_field(number, wire_type)!')
		e.wln(3, '}')
		e.wln(2, '}')
	}
	e.wln(1, '}')
	e.wln(0, '}')
	e.w('')
}

// emit_decode_case writes the `n { ... }` arm that reads one known field of `m`.
pub fn emit_decode_case(mut e Emitter, m ResolvedMessage, f Resolved) {
	n := f.number
	e.wln(3, '${n} {')
	// Reading a member of a `oneof` clears the others first. The spec says the
	// last member on the wire wins, so a payload carrying two members of one
	// group must not decode to both of them set.
	//
	// This is why the members are parallel optionals at all: nothing else in the
	// generated code knows which fields belong to the same group.
	for other in f.oneof_members {
		if other == f.name {
			continue
		}
		e.wln(4, 'out.${other} = none')
	}
	if f.kind == .map {
		// Each occurrence of a map field is exactly one entry, and the entries of
		// every occurrence add up, wherever in the message they appear. Reading
		// the entry through `sub` is also what makes a truncated one an error
		// rather than an empty map.
		e.wln(4, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.length_delimited)!')
		e.wln(4, 'mut sub := unpacker.sub()!')
		e.wln(4, 'sub.enter()!')
		e.wln(4, 'entry_key, entry_value := ${map_entry_read_fn_name(m.v_name, f.name)}(mut sub)!')
		e.wln(4, 'out.${f.name}[entry_key] = entry_value')
		e.wln(3, '}')
		return
	}
	if f.is_repeated() {
		emit_decode_repeated(mut e, f, n)
		e.wln(3, '}')
		return
	}
	if f.kind == .message {
		// The nested message starts from what an earlier occurrence left, so a
		// message split across two occurrences is merged as the spec asks.
		zero := if f.indirect { '&${f.elem_type}{}' } else { '${f.elem_type}{}' }
		e.wln(4, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.length_delimited)!')
		e.wln(4, 'mut sub := unpacker.sub()!')
		e.wln(4, 'sub.enter()!')
		e.wln(4, 'mut nested := out.${f.name} or { ${zero} }')
		e.wln(4, '${read_fn_name(f.elem_type)}(mut sub, mut nested)!')
		e.wln(4, 'out.${f.name} = nested')
		e.wln(3, '}')
		return
	}
	e.wln(4, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.${field_wire_type(f)})!')
	if f.kind == .text {
		e.wln(4, 'out.${f.name} = unpacker.read_string()!')
	} else if f.kind == .bytes {
		e.wln(4, 'out.${f.name} = unpacker.read_bytes()!')
	} else if f.kind == .enum {
		e.wln(4, 'out.${f.name} = unsafe { ${f.elem_type}(unpacker.read_enum()!) }')
	} else {
		e.wln(4, 'out.${f.name} = ${scalar_reader(f.scalar, 'unpacker')}!')
	}
	e.wln(3, '}')
}

// field_wire_type returns the wire type a field, or one element of a repeated
// field, arrives as.
//
// It keys off the field's kind rather than its scalar, because a string, a bytes
// field, and a nested message are all length-delimited whatever their V type is,
// and an enum is a varint because it travels as an integer. Only a genuine scalar
// takes its wire type from the ProtoScalar.
pub fn field_wire_type(f Resolved) protobuf.WireType {
	return match f.kind {
		.text, .bytes, .message { .length_delimited }
		.enum { .varint }
		else { f.scalar.wire_type() }
	}
}

// scalar_reader returns the call that reads one scalar from `reader`.
//
// The receiver is a parameter rather than a fixed `u`, because a scalar is read
// from the message unpacker in one place and from a map entry's sub-unpacker in
// another. Baking the receiver in produced `sub.u.read_int32()` for the second,
// which is not a call at all.
pub fn scalar_reader(s protobuf.ProtoScalar, reader string) string {
	return match s {
		.boolean { '${reader}.read_bool()' }
		.int32 { '${reader}.read_int32()' }
		.int64 { '${reader}.read_int64()' }
		.uint32 { '${reader}.read_uint32()' }
		.uint64 { '${reader}.read_uint64()' }
		.sint32 { '${reader}.read_sint32()' }
		.sint64 { '${reader}.read_sint64()' }
		.fixed32 { '${reader}.read_fixed32()' }
		.sfixed32 { '${reader}.read_sfixed32()' }
		.fixed64 { '${reader}.read_fixed64()' }
		.sfixed64 { '${reader}.read_sfixed64()' }
		.float32 { '${reader}.read_float()' }
		.float64 { '${reader}.read_double()' }
	}
}

// emit_decode_repeated writes the body of a list field's `n` arm.
//
// A numeric or enum element is accepted in both wire forms: the packed run the
// spec defaults to, and the unpacked one a producer is still allowed to emit.
// Any other wire type is the wrong one, and is reported rather than read as an
// element of the right one. A string, bytes, or message element has only the
// one length-delimited form.
pub fn emit_decode_repeated(mut e Emitter, f Resolved, n int) {
	if f.kind == .message {
		e.wln(4, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.length_delimited)!')
		e.wln(4, 'mut sub := unpacker.sub()!')
		e.wln(4, 'sub.enter()!')
		e.wln(4, 'mut item := ${f.elem_type}{}')
		e.wln(4, '${read_fn_name(f.elem_type)}(mut sub, mut item)!')
		e.wln(4, 'out.${f.name} << item')
		return
	}
	if !f.kind_supports_packed() {
		e.wln(4, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.length_delimited)!')
		e.wln(4, emit_decode_elem(f, 'unpacker'))
		return
	}
	e.wln(4, 'if wire_type == .length_delimited {')
	e.wln(5, '// a packed run: the elements back to back, with no tag of their own')
	e.wln(5, 'mut sub := unpacker.sub()!')
	e.wln(5, 'for !sub.eof() {')
	e.wln(6, emit_decode_elem(f, 'sub'))
	e.wln(5, '}')
	e.wln(4, '} else {')
	e.wln(5, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.${field_wire_type(f)})!')
	e.wln(5, emit_decode_elem(f, 'unpacker'))
	e.wln(4, '}')
}

// kind_supports_packed reports whether a length-delimited wire type on this
// field can mean "packed run" rather than "one element". Only a repeated numeric
// or enum field can be packed; a repeated string, bytes, or message arrives
// length-delimited as a single element and must be read that way.
//
// This asks whether the field *can* be packed, not whether this build packs it.
// The two are separate decisions: `[packed = false]` says what this producer
// emits, while a reader has to take a packed run from any producer, since the
// option is a hint about the writer and not part of the wire format. Asking
// `is_packed` here made the reader refuse exactly the payload a peer that ignored
// the option would send.
pub fn (f Resolved) kind_supports_packed() bool {
	return f.label == .repeated && (f.kind == .scalar || f.kind == .enum)
}

// emit_decode_elem returns the statement that appends one non-message element to
// the list being read.
pub fn emit_decode_elem(f Resolved, reader string) string {
	item := f.elem_type
	return match f.kind {
		.text { 'out.${f.name} << ${reader}.read_string()!' }
		.bytes { 'out.${f.name} << ${reader}.read_bytes()!' }
		.enum { 'out.${f.name} << unsafe { ${item}(${reader}.read_enum()!) }' }
		else { 'out.${f.name} << ${scalar_reader(f.scalar, reader)}!' }
	}
}

// emit_map_functions writes the encoder and the entry reader of every map field
// of `m`.
//
// A map is a repeated Entry message on the wire, with `key = 1` and `value = 2`,
// so each entry is a submessage rather than a field. The functions are emitted
// per field rather than shared so the generated code names the field it belongs
// to, which is what makes a failure traceable. They are named after the message
// as well as the field, since two messages may well both have a `labels` map.
pub fn emit_map_functions(mut e Emitter, m ResolvedMessage) {
	for f in m.fields {
		if f.kind != .map {
			continue
		}
		emit_map_encode(mut e, m, f)
		emit_map_entry_decode(mut e, m, f)
	}
}

// emit_map_encode writes the function that puts map field `f` of `m` on the wire.
pub fn emit_map_encode(mut e Emitter, m ResolvedMessage, f Resolved) {
	name := map_encode_fn_name(m.v_name, f.name)
	e.wln(0, '// ${name} writes `${f.name}` as the repeated Entry message the spec')
	e.wln(0, '// defines: one entry per pair, each with `key = 1` and `value = 2`.')
	e.wln(0, '//')
	e.wln(0, '// The entries are sorted by key. V map iteration order is undefined, so')
	e.wln(0, '// without the sort the same map would encode to different bytes on')
	e.wln(0, '// different runs, which breaks any comparison taken over the output.')
	e.wln(0, '// The options reach the entry packer so that a string key or a message value')
	e.wln(0, '// is held to the same rules as a field of the enclosing message.')
	e.wln(0, 'fn ${name}(mut packer protobuf.Packer, field_number int, map_data ${f.v_type}, opts protobuf.EncodeOpts) ! {')
	// The map is held in a local named `map_data` rather than `value`, because
	// `value` is a builtin type: a local of that name shadows it, and the checker
	// rejects the assignment that follows.
	e.wln(1, 'mut keys := map_data.keys()')
	e.wln(1, 'keys.sort()')
	e.wln(1, 'for entry_key in keys {')
	e.wln(2, 'entry_value := map_data[entry_key]')
	e.wln(2, 'mut entry := protobuf.new_packer(opts)')
	// The key's and the value's tests are worked out separately: a
	// map<string, int32> needs a length test for the key and a zero test for the
	// value, and reading both off the key type wrote the wrong test for one of
	// them.
	//
	// `emit_defaults` applies to an entry's members the same way it applies to a
	// field: without it, a member holding its default is left out of the entry.
	e.wln(2, 'if opts.emit_defaults || ${map_part_present(f, true, 'entry_key')} {')
	e.wln(3, emit_map_part_write(f, 'entry', 'entry_key', 1))
	e.wln(2, '}')
	if f.map_value_kind == .message {
		// A message value has presence, so it is written even when it is empty:
		// an entry without one would decode to the same empty message, but the
		// reference implementation writes it, and so does this.
		e.wln(2, emit_map_part_write(f, 'entry', 'entry_value', 2))
	} else {
		e.wln(2, 'if opts.emit_defaults || ${map_part_present(f, false, 'entry_value')} {')
		e.wln(3, emit_map_part_write(f, 'entry', 'entry_value', 2))
		e.wln(2, '}')
	}
	e.wln(2, 'packer.write_message(field_number, entry.bytes())')
	e.wln(1, '}')
	e.wln(0, '}')
	e.w('')
}

// map_part_present returns the condition under which a map entry's key or value
// is written. An entry that omits a scalar means it holds the default, so the
// same proto3 rule as a field applies: writing the default would be a byte the
// reference implementation does not need.
//
// It keys off the part's kind and scalar rather than its V type, because the V
// type of an enum or a message is just a name: `entry_value != 0` is not a test
// that compiles for either.
//
// The check is written as the condition for *writing* the part rather than for
// skipping it, so the caller can emit `if <here> {` without a negation. An
// earlier version returned the skip condition and the caller negated it, which
// produced `if !expr.len == 0`, and that parses as `(!expr.len) == 0` and is
// true for every non-empty string.
pub fn map_part_present(f Resolved, is_key bool, expr string) string {
	kind := if is_key { f.map_key_kind } else { f.map_value_kind }
	scalar := if is_key { f.map_key_scalar } else { f.scalar }
	return match kind {
		.text, .bytes {
			'${expr}.len > 0'
		}
		.enum {
			'int(${expr}) != 0'
		}
		.scalar {
			match scalar {
				.boolean { expr }
				.float32, .float64 { '${expr} != 0.0' }
				else { '${expr} != 0' }
			}
		}
		else {
			'true'
		}
	}
}

// emit_map_part_write returns the statement that writes one part of a map entry:
// the key when `n` is 1 and the value when it is 2.
pub fn emit_map_part_write(f Resolved, packer string, expr string, n int) string {
	is_key := n == 1
	kind := if is_key { f.map_key_kind } else { f.map_value_kind }
	return match kind {
		.text {
			'${packer}.write_string(${n}, ${expr})!'
		}
		.bytes {
			'${packer}.write_bytes(${n}, ${expr})'
		}
		.enum {
			'${packer}.write_enum(${n}, int(${expr}))'
		}
		.message {
			'${packer}.write_message(${n}, ${expr}.encode_with(opts)!)'
		}
		else {
			// The key's scalar comes from the schema's key type: `sint32`,
			// `fixed64` and the rest each have their own encoding, which the V type
			// alone cannot tell apart.
			scalar_write_call(packer, n, expr, if is_key { f.map_key_scalar } else { f.scalar })
		}
	}
}

// map_part_wire_type returns the wire type a map entry's key or value arrives as.
pub fn map_part_wire_type(f Resolved, is_key bool) protobuf.WireType {
	kind := if is_key { f.map_key_kind } else { f.map_value_kind }
	return match kind {
		.text, .bytes, .message { .length_delimited }
		.enum { .varint }
		else {
			if is_key { f.map_key_scalar.wire_type() } else { f.scalar.wire_type() }
		}
	}
}

// map_value_zero returns the expression a map entry's value starts from, which
// is what an entry that omits the value decodes to.
pub fn map_value_zero(f Resolved) string {
	if f.map_value_kind == .enum {
		// An enum's zero is a value, not a literal: `Color{}` is not V.
		return 'unsafe { ${f.map_value_type}(0) }'
	}
	return zero_value(f.map_value_type)
}

// scalar_write_call returns the Packer call that writes one scalar.
pub fn scalar_write_call(packer string, n int, expr string, s protobuf.ProtoScalar) string {
	return match s {
		.boolean { '${packer}.write_bool(${n}, ${expr})' }
		.int32 { '${packer}.write_int32(${n}, ${expr})' }
		.int64 { '${packer}.write_int64(${n}, ${expr})' }
		.uint32 { '${packer}.write_uint32(${n}, ${expr})' }
		.uint64 { '${packer}.write_uint64(${n}, ${expr})' }
		.sint32 { '${packer}.write_sint32(${n}, ${expr})' }
		.sint64 { '${packer}.write_sint64(${n}, ${expr})' }
		.fixed32 { '${packer}.write_fixed32(${n}, ${expr})' }
		.sfixed32 { '${packer}.write_sfixed32(${n}, ${expr})' }
		.fixed64 { '${packer}.write_fixed64(${n}, ${expr})' }
		.sfixed64 { '${packer}.write_sfixed64(${n}, ${expr})' }
		.float32 { '${packer}.write_float(${n}, ${expr})' }
		.float64 { '${packer}.write_double(${n}, ${expr})' }
	}
}

// emit_map_entry_decode writes the function that reads one entry of map field
// `f` of `m` and returns its key and value.
//
// The caller's `match` arm has already read the entry's tag and handed over an
// Unpacker over exactly the entry's payload, so this reads parts until that
// payload ends. Either part may be missing, and then holds its default: an Entry
// is a message, and proto3 leaves a default off the wire.
pub fn emit_map_entry_decode(mut e Emitter, m ResolvedMessage, f Resolved) {
	name := map_entry_read_fn_name(m.v_name, f.name)
	e.wln(0, '// ${name} reads one Entry message of `${f.name}` and returns its key')
	e.wln(0, "// and its value. A part the entry omits holds its type's zero, because an")
	e.wln(0, '// Entry missing a scalar means it holds the default rather than that the')
	e.wln(0, '// message is broken. A later entry with the same key replaces an earlier')
	e.wln(0, '// one, which is what the spec says, since the caller inserts each in turn.')
	e.wln(0, 'fn ${name}(mut unpacker protobuf.Unpacker) !(${f.map_key_type}, ${f.map_value_type}) {')
	// The locals are named `entry_key` and `entry_value` rather than `key` and
	// `value`: `value` is a builtin type, and a local of that name shadows it,
	// which the checker rejects on assignment.
	e.wln(1, 'mut entry_key := ${zero_value(f.map_key_type)}')
	e.wln(1, 'mut entry_value := ${map_value_zero(f)}')
	e.wln(1, 'for !unpacker.eof() {')
	e.wln(2, 'part, part_wire := unpacker.read_tag()!')
	e.wln(2, 'match part {')
	// Both parts have their wire type checked, as a field's is: a key sent as
	// the wrong type is a disagreement about the schema, not a key.
	key_wire := map_part_wire_type(f, true)
	value_wire := map_part_wire_type(f, false)
	e.wln(3, '1 {')
	e.wln(4, 'protobuf.check_wire_type(1, part_wire, protobuf.WireType.${key_wire})!')
	e.wln(4, emit_map_part_read(f, 'entry_key', 'unpacker', 1))
	e.wln(3, '}')
	e.wln(3, '2 {')
	e.wln(4, 'protobuf.check_wire_type(2, part_wire, protobuf.WireType.${value_wire})!')
	if f.map_value_kind == .message {
		e.wln(4, 'mut sub := unpacker.sub()!')
		e.wln(4, 'sub.enter()!')
		e.wln(4, '${read_fn_name(f.map_value_type)}(mut sub, mut entry_value)!')
	} else {
		e.wln(4, emit_map_part_read(f, 'entry_value', 'unpacker', 2))
	}
	e.wln(3, '}')
	e.wln(3, 'else {')
	e.wln(4, 'unpacker.skip_field(part, part_wire)!')
	e.wln(3, '}')
	e.wln(2, '}')
	e.wln(1, '}')
	e.wln(1, 'return entry_key, entry_value')
	e.wln(0, '}')
	e.w('')
}

// emit_map_part_read returns the statement that reads a map entry's key, when `n`
// is 1, or its non-message value, when it is 2.
pub fn emit_map_part_read(f Resolved, target string, reader string, n int) string {
	is_key := n == 1
	kind := if is_key { f.map_key_kind } else { f.map_value_kind }
	return match kind {
		.text {
			'${target} = ${reader}.read_string()!'
		}
		.bytes {
			'${target} = ${reader}.read_bytes()!'
		}
		.enum {
			'${target} = unsafe { ${f.map_value_type}(${reader}.read_enum()!) }'
		}
		else {
			s := if is_key { f.map_key_scalar } else { f.scalar }
			'${target} = ${scalar_reader(s, reader)}!'
		}
	}
}
