module main

import encoding.protobuf

// emit_message_decode writes `decode_<message>(data []u8) !T`.
pub fn emit_message_decode(mut e Emitter, m ResolvedMessage) {
	fn_name := 'decode_${snake_case(m.v_name)}'
	e.wln(0, '// ${fn_name} parses the proto3 message `${m.doc_name}`.')
	e.wln(0, '//')
	e.wln(0, '// A field this build does not know is skipped rather than rejected, because')
	e.wln(0, '// forward compatibility is a requirement of the format: a newer producer')
	e.wln(0, '// must not break an older consumer. A field that is known but arrives with')
	e.wln(0, '// the wrong wire type is an error, since that is a real disagreement')
	e.wln(0, '// rather than a field from the future.')
	e.wln(0, 'pub fn ${fn_name}(data []u8) !${m.v_name} {')
	e.wln(1, 'mut out := ${m.v_name}{}')
	e.wln(1, 'mut u := protobuf.new_unpacker(data, protobuf.DecodeOpts{})')
	if m.fields.len == 0 {
		e.wln(1, '_ = &u')
		e.wln(1, '_ = &out')
	} else {
		e.wln(1, 'for !u.eof() {')
		e.wln(2, 'number, wire_type := u.read_tag()!')
		e.wln(2, 'match number {')
		for f in m.fields {
			emit_decode_case(mut e, f)
		}
		e.wln(3, 'else {')
		e.wln(4, '// an unknown field, or one this build does not know')
		e.wln(4, 'u.skip_field(number, wire_type)!')
		e.wln(3, '}')
		e.wln(2, '}')
		e.wln(1, '}')
	}
	e.wln(1, 'return out')
	e.wln(0, '}')
	e.w('')
}

// emit_decode_case writes the `n { ... }` arm that reads one known field.
pub fn emit_decode_case(mut e Emitter, f Resolved) {
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
		e.wln(4, 'out.${f.name} = decode_map_${f.name}(mut u, ${n}, wire_type)!')
		e.wln(3, '}')
		return
	}
	if f.is_repeated() {
		emit_decode_repeated(mut e, f, n)
		e.wln(3, '}')
		return
	}
	if f.kind == .message {
		e.wln(4, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.length_delimited)!')
		e.wln(4, 'payload := u.read_len_delimited()!')
		e.wln(4, 'out.${f.name} = decode_${snake_case(f.elem_type)}(payload)!')
		e.wln(3, '}')
		return
	}
	e.wln(4, 'protobuf.check_wire_type(${n}, wire_type, protobuf.WireType.${field_wire_name(f)})!')
	if f.kind == .text {
		e.wln(4, 'out.${f.name} = u.read_string()!')
	} else if f.kind == .bytes {
		e.wln(4, 'out.${f.name} = u.read_bytes()!')
	} else if f.kind == .enum {
		e.wln(4, 'out.${f.name} = unsafe { ${f.elem_type}(u.read_enum()!) }')
	} else {
		e.wln(4, 'out.${f.name} = ${scalar_reader(f.scalar, 'u')}!')
	}
	e.wln(3, '}')
}

// field_wire_name returns the V spelling of the wire type a field arrives as.
//
// It keys off the field's kind rather than its scalar, because a string, a bytes
// field, and a nested message are all length-delimited whatever their V type is,
// and an enum is a varint because it travels as an integer. Only a genuine scalar
// takes its wire type from the ProtoScalar.
pub fn field_wire_name(f Resolved) string {
	return match f.kind {
		.text, .bytes, .message { 'length_delimited' }
		.enum { 'varint' }
		else { scalar_wire_name(f.scalar) }
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
// Both wire forms are accepted: the packed run the spec defaults to, and the
// unpacked one a producer is still allowed to emit. Deciding between them is the
// whole of the complexity here, and getting it wrong would make a payload from an
// older producer unreadable.
pub fn emit_decode_repeated(mut e Emitter, f Resolved, n int) {
	e.wln(4, 'if wire_type == .length_delimited && ${f.kind_supports_packed()} {')
	e.wln(5, 'payload := u.read_len_delimited()!')
	e.wln(5, 'mut sub := protobuf.new_unpacker(payload, protobuf.DecodeOpts{})')
	e.wln(5, 'for !sub.eof() {')
	packed_line := emit_decode_elem(f, 'sub')
	if packed_line != '' {
		e.wln(6, packed_line)
	}
	e.wln(5, '}')
	e.wln(4, '} else {')
	plain_line := emit_decode_elem(f, 'u')
	if plain_line != '' {
		e.wln(5, plain_line)
	}
	e.wln(4, '}')
}

// kind_supports_packed returns the condition under which a length-delimited wire
// type means "packed run" rather than "one element". Only a repeated numeric
// field can be packed; a repeated string, bytes, or message arrives
// length-delimited as a single element and must be read that way.
pub fn (f Resolved) kind_supports_packed() string {
	return if f.is_packed() { 'true' } else { 'false' }
}

// emit_decode_elem returns the statement that appends one element to the field
// being read, or an empty string when the element is a message and the read has
// already been spelled out.
pub fn emit_decode_elem(f Resolved, reader string) string {
	item := f.elem_type
	match f.kind {
		.text { return 'out.${f.name} << ${reader}.read_string()!' }
		.bytes { return 'out.${f.name} << ${reader}.read_bytes()!' }
		.message {
			return 'out.${f.name} << decode_${snake_case(item)}(${reader}.read_len_delimited()!)!'
		}
		.enum {
			return 'out.${f.name} << unsafe { ${item}(${reader}.read_enum()!) }'
		}
		.scalar { return 'out.${f.name} << ${scalar_reader(f.scalar, reader)}!' }
		else { return '' }
	}
}

// emit_map_functions writes the per-field map encoder and decoder.
//
// A map is a repeated Entry message on the wire, with `key = 1` and `value = 2`,
// so each entry is a submessage rather than a field. The functions are emitted
// per field rather than shared so the generated code names the field it belongs
// to, which is what makes a failure traceable.
pub fn emit_map_functions(mut e Emitter, m ResolvedMessage) {
	for f in m.fields {
		if f.kind != .map {
			continue
		}
		emit_map_encode(mut e, f)
		emit_map_decode(mut e, f)
	}
}

// emit_map_encode writes the function that puts a map field on the wire.
pub fn emit_map_encode(mut e Emitter, f Resolved) {
	e.wln(0, '// emit_map_${f.name} writes `${f.name}` as the repeated Entry message the spec')
	e.wln(0, '// defines: one entry per pair, each with `key = 1` and `value = 2`.')
	e.wln(0, '//')
	e.wln(0, '// The entries are sorted by key. V map iteration order is undefined, so')
	e.wln(0, '// without the sort the same map would encode to different bytes on')
	e.wln(0, '// different runs, which breaks any comparison taken over the output.')
	e.wln(0, 'pub fn emit_map_${f.name}(mut p protobuf.Packer, field_number int, value ${f.v_type}) ! {')
	e.wln(1, 'mut keys := value.keys()')
	e.wln(1, 'keys.sort()')
	e.wln(1, 'for entry_key in keys {')
	e.wln(2, 'entry_value := value[entry_key]')
	e.wln(2, 'mut entry := protobuf.new_packer(protobuf.EncodeOpts{})')
	// The key's and the value's types are checked separately: a map<string, int32>
	// needs a length test for the key and a zero test for the value, and reading
	// both off the key type wrote the wrong test for one of them.
	e.wln(2, 'if ${is_map_default(f, f.map_key_type, 'entry_key')} {')
	key_line := emit_map_part_write(f, 'entry', 'entry_key', 1)
	if key_line != '' {
		e.wln(3, key_line)
	}
	e.wln(2, '}')
	e.wln(2, 'if ${is_map_default(f, f.map_value_type, 'entry_value')} {')
	value_line := emit_map_part_write(f, 'entry', 'entry_value', 2)
	if value_line != '' {
		e.wln(3, value_line)
	}
	e.wln(2, '}')
	e.wln(2, 'p.write_message(field_number, entry.bytes())')
	e.wln(1, '}')
	e.wln(0, '}')
	e.w('')
}

// is_map_default returns the condition under which an Entry's member is absent.
// An Entry that omits a scalar means it holds the default, so the same proto3
// rule as a field applies: writing the default would be a byte the reference
// implementation does not write.
//
// The check is written as the condition for *writing* the member rather than for
// skipping it, so the caller can emit `if <here> {` without a negation. An
// earlier version returned the skip condition and the caller negated it, which
// produced `if !expr.len == 0`, and that parses as `(!expr.len) == 0` and is
// true for every non-empty string.
pub fn is_map_default(f Resolved, v_type string, expr string) string {
	return match v_type {
		'string' { '${expr}.len > 0' }
		'bool' { '${expr}' }
		'f32', 'f64' { '${expr} != 0.0' }
		else { '${expr} != 0' }
	}
}

// emit_map_part_write returns the statement that writes one member of a map
// entry, or an empty string when the member is a message and the read has to be
// a nested call.
pub fn emit_map_part_write(f Resolved, packer string, expr string, n int) string {
	is_key := n == 1
	kind := if is_key {
		kind_for_map_key(f.map_key_type)
	} else {
		f.map_value_kind
	}
	match kind {
		.text { return '${packer}.write_string(${n}, ${expr})!' }
		.bytes { return '${packer}.write_bytes(${n}, ${expr})' }
		.enum { return '${packer}.write_enum(${n}, int(${expr}))' }
		.scalar {
			s := if is_key {
				scalar_for_map_key(f.map_key_type)
			} else {
				f.scalar
			}
			return scalar_write_call(packer, n, expr, s)
		}
		else { return '' }
	}
}

// kind_for_map_key returns the FieldKind a V map key type stands for.
pub fn kind_for_map_key(v_type string) FieldKind {
	return match v_type {
		'string' { .text }
		else { .scalar }
	}
}

// scalar_for_map_key returns the ProtoScalar a V map key type stands for, since
// a map key is written as the plain integral type its V type implies.
pub fn scalar_for_map_key(v_type string) protobuf.ProtoScalar {
	return match v_type {
		'bool' { .boolean }
		'i32' { .int32 }
		'i64' { .int64 }
		'u32' { .uint32 }
		'u64' { .uint64 }
		else { .int32 }
	}
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

// emit_map_decode writes the function that reads a map field.
pub fn emit_map_decode(mut e Emitter, f Resolved) {
	e.wln(0, '// decode_map_${f.name} reads a repeated Entry message into `${f.name}`.')
	e.wln(0, '//')
	e.wln(0, '// A later entry with the same key replaces the earlier one, which is what')
	e.wln(0, "// the spec says. An entry that omits a member takes that member's zero,")
	e.wln(0, '// because an Entry missing a scalar means it holds the default rather')
	e.wln(0, '// than that the message is broken.')
	e.wln(0, '//')
	e.wln(0, '// `field_number` and `wire_type` are the already-read tag of the first entry,')
	e.wln(0, "// passed in so the map's wire type is checked against the field rather than")
	e.wln(0, '// inferred again.')
	// The caller's `match` arm has already read the tag for this occurrence of the
	// field, so the first entry's payload is read here rather than by reading
	// another tag. Reading a tag first consumed the entry's own length prefix as
	// if it were a tag, and decoding then failed with "field number 0".
	//
	// After that first entry, any further occurrence of the field does have its own
	// tag ahead of it, so the loop peeks for one and hands it back when it
	// belongs to a different field. That is what lets a map be followed by
	// another field in the same message.
	e.wln(0, 'pub fn decode_map_${f.name}(mut u protobuf.Unpacker, field_number int, wire_type protobuf.WireType) !${f.v_type} {')
	e.wln(1, 'protobuf.check_wire_type(field_number, wire_type, protobuf.WireType.length_delimited)!')
	e.wln(1, 'mut out := ${f.v_type}{}')
	e.wln(1, '// The outer loop runs once per entry the wire carries for this field. The')
	e.wln(1, '// first entry has no tag ahead of it, so it is read before the loop and')
	e.wln(1, '// every later one is read at the bottom, which is where the next tag is')
	e.wln(1, '// found and checked.')
	e.wln(1, 'mut first := true')
	e.wln(1, 'for {')
	e.wln(2, 'if !first {')
	e.wln(3, 'if u.eof() {')
	e.wln(4, 'break')
	e.wln(3, '}')
	e.wln(3, 'at := u.offset()')
	e.wln(3, 'number, entry_wire := u.read_tag()!')
	e.wln(3, 'if number != field_number {')
	e.wln(4, '// a different field starts here, so hand the tag back')
	e.wln(4, 'u.seek(at)')
	e.wln(4, 'break')
	e.wln(3, '}')
	e.wln(3, 'protobuf.check_wire_type(field_number, entry_wire, protobuf.WireType.length_delimited)!')
	e.wln(2, '}')
	e.wln(2, 'first = false')
	e.wln(2, 'if u.eof() {')
	e.wln(3, 'break')
	e.wln(2, '}')
	e.wln(2, 'mut sub := u.sub()!')
	// The locals are named `entry_key` and `entry_value` rather than `key` and
	// `value`: `value` is a builtin type, and a local of that name shadows it,
	// which the checker rejects on assignment.
	e.wln(2, 'mut entry_key := ${zero_value(f.map_key_type)}')
	e.wln(2, 'mut entry_value := ${zero_value(f.map_value_type)}')
	e.wln(2, 'for !sub.eof() {')
	e.wln(3, 'part, part_wire := sub.read_tag()!')
	e.wln(3, 'match part {')
	e.wln(4, '1 {')
	key_line := emit_map_part_read(f, 'entry_key', 'sub', 1)
	if key_line != '' {
		e.wln(5, key_line)
	}
	e.wln(4, '}')
	e.wln(4, '2 {')
	value_line := emit_map_part_read(f, 'entry_value', 'sub', 2)
	if value_line != '' {
		e.wln(5, value_line)
	}
	e.wln(4, '}')
	e.wln(4, 'else {')
	e.wln(5, 'sub.skip_field(part, part_wire)!')
	e.wln(4, '}')
	e.wln(3, '}')
	e.wln(2, '}')
	e.wln(2, 'out[entry_key] = entry_value')
	e.wln(1, '}')
	e.wln(1, 'return out')
	e.wln(0, '}')
	e.w('')
}

// emit_map_part_read returns the statement that reads one member of a map entry.
pub fn emit_map_part_read(f Resolved, target string, reader string, n int) string {
	is_key := n == 1
	kind := if is_key {
		kind_for_map_key(f.map_key_type)
	} else {
		f.map_value_kind
	}
	match kind {
		.text { return '${target} = ${reader}.read_string()!' }
		.bytes { return '${target} = ${reader}.read_bytes()!' }
		.enum {
			t := if is_key {
				f.map_key_type
			} else {
				f.map_value_type
			}
			return '${target} = unsafe { ${t}(${reader}.read_enum()!) }'
		}
		.scalar {
			s := if is_key {
				scalar_for_map_key(f.map_key_type)
			} else {
				f.scalar
			}
			return '${target} = ${scalar_reader(s, reader)}!'
		}
		.message {
			t := if is_key {
				f.map_key_type
			} else {
				f.map_value_type
			}
			return '${target} = decode_${snake_case(t)}(${reader}.read_len_delimited()!)!'
		}
		else { return '' }
	}
}
