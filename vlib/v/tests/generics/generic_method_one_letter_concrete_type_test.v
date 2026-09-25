import onelettercodec

// A one-letter struct name is a concrete type, not a generic placeholder, when
// it is the type argument of a generic method specialization (issue #28829).
struct F {
	x int
}

struct Box[T] {
	value T
}

fn (b Box[T]) name() string {
	return typeof(b.value).name
}

struct Enc {}

fn (e Enc) name[T](v T) string {
	return typeof(v).name
}

fn local_name[T](v T) string {
	return Enc{}.name(v)
}

fn test_imported_generic_fn_calling_generic_method() {
	assert onelettercodec.encode(F{1}) == 'F'
}

fn test_imported_generic_method() {
	e := onelettercodec.Encoder{}
	assert e.encode(F{1}) == 'F'
	wrapped := e.wrap(F{7})
	assert wrapped.len == 1
	assert wrapped[0].x == 7
}

fn test_imported_generic_fn() {
	assert onelettercodec.name(F{1}) == 'F'
}

fn test_imported_generic_method_with_composite_types() {
	e := onelettercodec.Encoder{}
	assert e.encode([F{1}]) == '[]F'
	o := ?F(F{2})
	assert onelettercodec.encode(o) == '?F'
	assert onelettercodec.encode({
		'a': F{3}
	}) == 'map[string]F'
}

fn test_local_generic_method() {
	assert Enc{}.name(F{1}) == 'F'
	assert local_name(F{2}) == 'F'
}

fn test_generic_struct_methods() {
	b := Box[F]{
		value: F{1}
	}
	assert b.name() == 'F'
	h := onelettercodec.Holder[F]{
		value: F{2}
	}
	assert h.name() == 'F'
}
