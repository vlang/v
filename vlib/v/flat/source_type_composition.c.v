@[has_globals]
module flat

import strings

// SourceTypeComposition describes a spelling constructor over source spelling IDs.
// These are formatting operations, independent of semantic type resolution.
pub enum SourceTypeComposition {
	pointer
	array
	option
	result
	channel
	mutable_channel
	map
	fixed_array
	fixed_array_suffix
	fixed_array_size
	fixed_array_size_suffix
	function_type
	void_function_type
	multi_return
	mutable_parameter
	variadic_parameter
	shared_type
	atomic_type
	generic
	qualified
	thread
	function_type_spaced
	void_function_type_spaced
	named_parameter
	concat
	inferred_expression
	fastc_thread_handle
	fastc_composite_part
	legacy_mut_chars
}

struct SourceTypeCompositionEntry {
	kind  SourceTypeComposition
	hash  u64
	parts &u32 = unsafe { nil }
	len   int
	id    u32
	next  &SourceTypeCompositionEntry = unsafe { nil }
}

__global g_source_type_compositions [4096]voidptr

// source_type_composition reuses a spelling built from an exact numeric tuple.
// Unary constructors take one part; maps take key/value; fixed arrays take the
// exact length spelling and element; fixed_array_size variants take the low and
// high words of a signed 64-bit length, then the element spelling. Functions take parameter spellings followed
// by the result; void_function_type takes only parameters. Multiple returns take
// their component spellings. Generics take a base followed by arguments;
// qualified spellings take a module and name. Shared and atomic are unary.
// Concatenation accepts any number of parts, including an empty tuple.
// inferred_expression takes raw expression-node bits instead of a spelling ID.
// fastc_thread_handle encodes one source spelling into its FastC thread handle name.
// fastc_composite_part applies FastC component escaping only when that spelling is requested.
// legacy_mut_chars removes leading raw bytes from the character set `m`, `u`, `t`, and ASCII space.
// Entries own only IDs, so borrowed type descriptors and worker arenas can change
// or expire without invalidating a cached spelling.
@[direct_array_access]
pub fn source_type_composition(kind SourceTypeComposition, parts []u32) u32 {
	mut hash := u64(kind) + u64(14_695_981_039_346_656_037)
	for part in parts {
		hash = (hash ^ u64(part)) * u64(1_099_511_628_211)
	}
	hash ^= hash >> 32
	slot := int(hash & 4095)
	cached := source_type_composition_find(slot, kind, parts, hash)
	if !isnil(cached) {
		return cached.id
	}
	return source_type_composition_miss(slot, kind, parts, hash)
}

// source_type_composition_one looks up a unary spelling without allocating a tuple.
@[inline]
pub fn source_type_composition_one(kind SourceTypeComposition, part u32) u32 {
	parts := [part]!
	// The lookup consumes this stack view synchronously and copies keys on a miss.
	return source_type_composition(kind, unsafe {
		array{ data: &parts[0], len: parts.len, cap: parts.len, element_size: sizeof(u32), flags: .nogrow | .nofree }
	})
}

// source_type_composition_two looks up a binary spelling without allocating a tuple.
@[inline]
pub fn source_type_composition_two(kind SourceTypeComposition, first u32, second u32) u32 {
	parts := [first, second]!
	// The lookup consumes this stack view synchronously and copies keys on a miss.
	return source_type_composition(kind, unsafe {
		array{ data: &parts[0], len: parts.len, cap: parts.len, element_size: sizeof(u32), flags: .nogrow | .nofree }
	})
}

// source_type_fixed_array reuses prefix or method-receiver suffix spellings.
// Numeric lengths stay numeric on hits, including targets where int is 64 bits.
pub fn source_type_fixed_array(length int, expression string, element u32, suffix bool) u32 {
	if expression.len > 0 {
		kind := if suffix {
			SourceTypeComposition.fixed_array_suffix
		} else {
			SourceTypeComposition.fixed_array
		}
		return source_type_composition_two(kind, source_type_id(expression), element)
	}
	bits := u64(i64(length))
	parts := [u32(bits), u32(bits >> 32), element]!
	kind := if suffix {
		SourceTypeComposition.fixed_array_size_suffix
	} else {
		SourceTypeComposition.fixed_array_size
	}
	// The cache copies this stack tuple only on a miss.
	return source_type_composition(kind, unsafe {
		array{ data: &parts[0], len: parts.len, cap: parts.len, element_size: sizeof(u32), flags: .nogrow | .nofree }
	})
}

@[direct_array_access; inline]
fn source_type_composition_head(slot int) &SourceTypeCompositionEntry {
	$if x32 {
		return unsafe { &SourceTypeCompositionEntry(voidptr(C.atomic_load_u32(voidptr(&g_source_type_compositions[slot])))) }
	} $else {
		return unsafe { &SourceTypeCompositionEntry(voidptr(C.atomic_load_u64(voidptr(&g_source_type_compositions[slot])))) }
	}
}

@[direct_array_access]
fn source_type_composition_find(slot int, kind SourceTypeComposition, parts []u32, hash u64) &SourceTypeCompositionEntry {
	mut entry := source_type_composition_head(slot)
	for !isnil(entry) {
		if entry.hash == hash && entry.kind == kind && entry.len == parts.len {
			mut equal := true
			for index, part in parts {
				if unsafe { entry.parts[index] } != part {
					equal = false
					break
				}
			}
			if equal {
				return entry
			}
		}
		entry = entry.next
	}
	return unsafe { nil }
}

fn source_type_composition_miss(slot int, kind SourceTypeComposition, parts []u32, hash u64) u32 {
	// Rendering interns a spelling and must finish before taking its writer lock.
	id := source_type_id(source_type_composition_text(kind, parts))
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	cached := source_type_composition_find(slot, kind, parts, hash)
	if !isnil(cached) {
		return cached.id
	}
	mut entry := unsafe { &SourceTypeCompositionEntry(node_payload_alloc_zeroed(sizeof(SourceTypeCompositionEntry))) }
	if isnil(entry) {
		panic('could not allocate a composite source type')
	}
	mut owned := unsafe { &u32(nil) }
	if parts.len > 0 {
		owned = unsafe { &u32(node_payload_alloc_zeroed(uint(parts.len) * sizeof(u32))) }
		if isnil(owned) {
			panic('could not allocate composite source type parts')
		}
		unsafe { vmemcpy(owned, parts.data, parts.len * int(sizeof(u32))) }
	}
	// Process-owned immutable entries are visible only after the release store.
	unsafe {
		*entry = SourceTypeCompositionEntry{
			kind:  kind
			hash:  hash
			parts: owned
			len:   parts.len
			id:    id
			next:  source_type_composition_head(slot)
		}
	}
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_compositions[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_compositions[slot]), u64(voidptr(entry)))
	}
	return id
}

fn source_type_composition_text(kind SourceTypeComposition, parts []u32) string {
	match kind {
		.pointer { return '&${source_type_name(parts[0])}' }
		.array { return '[]${source_type_name(parts[0])}' }
		.option { return '?${source_type_name(parts[0])}' }
		.result { return '!${source_type_name(parts[0])}' }
		.channel { return 'chan ${source_type_name(parts[0])}' }
		.mutable_channel { return 'chan mut ${source_type_name(parts[0])}' }
		.mutable_parameter { return 'mut ${source_type_name(parts[0])}' }
		.variadic_parameter { return '...${source_type_name(parts[0])}' }
		.shared_type { return 'shared ${source_type_name(parts[0])}' }
		.atomic_type { return 'atomic ${source_type_name(parts[0])}' }
		.qualified { return '${source_type_name(parts[0])}.${source_type_name(parts[1])}' }
		.thread { return 'thread ${source_type_name(parts[0])}' }
		.named_parameter { return '${source_type_name(parts[0])} ${source_type_name(parts[1])}' }
		.inferred_expression { return 'typeof(__expr_${int(parts[0])})' }
		.legacy_mut_chars { return source_type_name(parts[0]).trim_left('mut ') }
		.fastc_composite_part {
			text := source_type_name(parts[0])
			return if text.contains(' ') || text.contains('*') || text.contains('.') {
				text.replace(' ', '_').replace('*', '_ptr').replace('.', '__')
			} else {
				text
			}
		}
		.fastc_thread_handle {
			value_type := source_type_name(parts[0])
			if value_type in ['', 'void'] { return '__vf_thread_void' }
			hex_digits := '0123456789abcdef'
			mut text := strings.new_builder(value_type.len + 16)
			text.write_string('__vf_thread_k')
			for value in value_type {
				if (value >= `a` && value <= `z`) || (value >= `A` && value <= `Z`)
					|| (value >= `0` && value <= `9`) {
					text.write_u8(value)
				} else {
					text.write_u8(`_`)
					text.write_u8(hex_digits[value >> 4])
					text.write_u8(hex_digits[value & 0x0f])
				}
			}
			return text.str()
		}
		.concat {
			mut text := strings.new_builder(32 + parts.len * 8)
			for part in parts {
				text.write_string(source_type_name(part))
			}
			return text.str()
		}
		.generic {
			mut text := strings.new_builder(32 + parts.len * 8)
			text.write_string(source_type_name(parts[0]))
			text.write_u8(`[`)
			for index in 1 .. parts.len {
				if index > 1 { text.write_string(', ') }
				text.write_string(source_type_name(parts[index]))
			}
			text.write_u8(`]`)
			return text.str()
		}
		.map { return 'map[${source_type_name(parts[0])}]${source_type_name(parts[1])}' }
		.fixed_array { return '[${source_type_name(parts[0])}]${source_type_name(parts[1])}' }
		.fixed_array_suffix {
			return '${source_type_name(parts[1])}[${source_type_name(parts[0])}]'
		}
		.fixed_array_size {
			return '[${i64(u64(parts[0]) | (u64(parts[1]) << 32))}]${source_type_name(parts[2])}'
		}
		.fixed_array_size_suffix {
			return '${source_type_name(parts[2])}[${i64(u64(parts[0]) | (u64(parts[1]) << 32))}]'
		}
		.function_type, .void_function_type, .function_type_spaced, .void_function_type_spaced,
		.multi_return {
			mut text := strings.new_builder(32 + parts.len * 8)
			has_result := kind == .function_type || kind == .function_type_spaced
			is_function := kind != .multi_return
			text.write_string(if kind == .function_type_spaced || kind == .void_function_type_spaced {
				'fn ('
			} else if is_function {
				'fn('
			} else {
				'('
			})
			count := parts.len - if has_result { 1 } else { 0 }
			for index in 0 .. count {
				if index > 0 { text.write_string(', ') }
				text.write_string(source_type_name(parts[index]))
			}
			text.write_u8(`)`)
			if has_result {
				text.write_u8(` `)
				text.write_string(source_type_name(parts[count]))
			}
			return text.str()
		}
	}
}
