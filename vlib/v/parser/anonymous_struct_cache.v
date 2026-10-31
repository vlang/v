module parser

// Anonymous struct field names stay textual, while type identities stay numeric.
struct AnonymousStructTypedShape {
	field_names []string
	field_types []u32
mut:
	candidates []u32
}

struct AnonymousStructNameShape {
	field_names []string
mut:
	candidates []u32
}

struct AnonymousStructShapeCache {
mut:
	typed_shapes  []AnonymousStructTypedShape
	typed_buckets map[u64][]int
	name_shapes   []AnonymousStructNameShape
	name_buckets  map[u64][]int
}

fn anonymous_struct_name_shape_hash(fields []string) u64 {
	mut hashed := u64(0xcbf29ce484222325) ^ u64(fields.len)
	for field in fields {
		hashed = (hashed ^ u64(field.len)) * u64(1099511628211)
		for ch in field {
			hashed = (hashed ^ u64(ch)) * u64(1099511628211)
		}
	}
	return hashed
}

fn anonymous_struct_typed_shape_hash(fields []string, field_types []u32) u64 {
	mut hashed := anonymous_struct_name_shape_hash(fields)
	for i in 0 .. fields.len {
		typ := if i < field_types.len { field_types[i] } else { u32(0) }
		hashed = (hashed ^ u64(typ)) * u64(1099511628211)
	}
	return hashed
}

fn (cache &AnonymousStructShapeCache) typed_shape_index(hashed u64, fields []string, field_types []u32) ?int {
	for index in cache.typed_buckets[hashed] {
		shape := cache.typed_shapes[index]
		if shape.field_names != fields {
			continue
		}
		mut matches := true
		for i, typ in shape.field_types {
			actual := if i < field_types.len { field_types[i] } else { u32(0) }
			if typ != actual {
				matches = false
				break
			}
		}
		if matches {
			return index
		}
	}
	return none
}

fn (cache &AnonymousStructShapeCache) name_shape_index(hashed u64, fields []string) ?int {
	for index in cache.name_buckets[hashed] {
		if cache.name_shapes[index].field_names == fields {
			return index
		}
	}
	return none
}

fn (cache &AnonymousStructShapeCache) typed_candidates(fields []string, field_types []u32) ?[]u32 {
	index := cache.typed_shape_index(anonymous_struct_typed_shape_hash(fields, field_types), fields,
		field_types) or { return none }
	return cache.typed_shapes[index].candidates
}

fn (cache &AnonymousStructShapeCache) name_candidates(fields []string) ?[]u32 {
	index := cache.name_shape_index(anonymous_struct_name_shape_hash(fields), fields) or {
		return none
	}
	return cache.name_shapes[index].candidates
}

fn (mut cache AnonymousStructShapeCache) append_typed(fields []string, field_types []u32, candidate u32) {
	hashed := anonymous_struct_typed_shape_hash(fields, field_types)
	if index := cache.typed_shape_index(hashed, fields, field_types) {
		cache.typed_shapes[index].candidates << candidate
		return
	}
	mut owned_fields := []string{cap: fields.len}
	for field in fields {
		owned_fields << field.clone()
	}
	// Match the original key: pad missing field types with zero and ignore extras.
	mut owned_types := []u32{len: fields.len}
	for i in 0 .. fields.len {
		if i < field_types.len {
			owned_types[i] = field_types[i]
		}
	}
	index := cache.typed_shapes.len
	cache.typed_shapes << AnonymousStructTypedShape{
		field_names: owned_fields
		field_types: owned_types
		candidates:  [candidate]
	}
	cache.typed_buckets[hashed] << index
}

fn (mut cache AnonymousStructShapeCache) append_declared(fields []string, candidate u32) {
	hashed := anonymous_struct_name_shape_hash(fields)
	if index := cache.name_shape_index(hashed, fields) {
		cache.name_shapes[index].candidates << candidate
		return
	}
	mut owned_fields := []string{cap: fields.len}
	for field in fields {
		owned_fields << field.clone()
	}
	index := cache.name_shapes.len
	cache.name_shapes << AnonymousStructNameShape{
		field_names: owned_fields
		candidates:  [candidate]
	}
	cache.name_buckets[hashed] << index
}
