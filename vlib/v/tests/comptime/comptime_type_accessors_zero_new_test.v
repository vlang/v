import comptime_accessor_helper

struct PrivatePayload {
	n int
}

struct Box {
	n int
}

struct Issue26995Foo {
	name string
}

struct Issue26996Options {
mut:
	a ?int
	b ?string
}

struct Issue26996OptionsReversed {
mut:
	a ?string
	b ?int
}

type ExampleSum = int | string | Box

fn maybe_private_payload() ?PrivatePayload {
	return PrivatePayload{
		n: 7
	}
}

fn result_string() !string {
	return 'abc'
}

fn maybe_int_ptr() ?&int {
	return none
}

fn result_int_ptr() !&int {
	return error('no pointer')
}

fn payload_idx[T](x T) int {
	return typeof(x).payload_type
}

fn option_payload_idx[T](x ?T) int {
	return typeof(x).payload_type
}

fn pointee_idx[T](x T) int {
	return typeof(x).pointee_type
}

fn variant_type_idxs[T]() []int {
	return T.variant_types
}

fn issue26995_make_pointer[T]() T {
	$if T.indirections == 1 {
		return $new(T.pointee_type)
	} $else {
		return $zero(T)
	}
}

fn issue26995_make_pointer_via_local[T]() T {
	$if T.indirections == 1 {
		mut p := $new(T.pointee_type)
		return p
	} $else {
		return $zero(T)
	}
}

fn issue26996_fill[T](mut x T) {
	$for field in T.fields {
		$if field.typ is $option {
			v := $zero(field.typ.payload_type)
			x.$(field.name) = v
		}
	}
}

fn issue26995_zero_payload_from_typeof[T](x ?T) T {
	return $zero(typeof(x).payload_type)
}

fn issue26995_new_payload_from_typeof[T](x ?T) &T {
	return $new(typeof(x).payload_type)
}

fn test_payload_type_accessor() {
	opt := maybe_private_payload()
	assert typeof(opt).payload_type == typeof[PrivatePayload]().idx
	assert option_payload_idx(opt) == typeof[PrivatePayload]().idx
	assert typeof(result_string()).payload_type == typeof[string]().idx
}

fn test_pointee_type_accessor() {
	value := 123
	ptr := &value
	assert typeof(ptr).pointee_type == typeof[int]().idx
	assert pointee_idx(ptr) == typeof[int]().idx
	assert typeof(maybe_int_ptr()).pointee_type == typeof[int]().idx
	assert typeof(result_int_ptr()).pointee_type == typeof[int]().idx
	assert typeof(maybe_int_ptr()).payload_type == typeof[&int]().idx
	assert typeof(maybe_int_ptr()).payload_type.pointee_type == typeof[int]().idx
}

fn test_variant_types_accessor() {
	idxs := variant_type_idxs[ExampleSum]()
	assert idxs == [typeof[int]().idx, typeof[string]().idx, typeof[Box]().idx]
	assert typeof[ExampleSum]().variant_types == idxs
}

fn test_zero_and_new() {
	opt := maybe_private_payload()
	zero_int := $zero(int)
	zero_payload := $zero(typeof(opt).payload_type)
	zero_pointee := $zero(typeof(maybe_int_ptr()).payload_type.pointee_type)
	zero_array := $zero([]typeof(opt).payload_type{})
	new_int := $new(int)
	new_payload := $new(typeof(opt).payload_type)
	new_pointee := $new(typeof(result_int_ptr()).payload_type.pointee_type)

	assert zero_int == 0
	assert zero_payload == PrivatePayload{}
	assert zero_pointee == 0
	assert zero_array.len == 0
	assert *new_int == 0
	assert *new_payload == PrivatePayload{}
	assert *new_pointee == 0
}

fn test_new_t_pointee_type_is_per_generic_instantiation() {
	int_ptr := issue26995_make_pointer[&int]()
	foo_ptr := issue26995_make_pointer[&Issue26995Foo]()
	int_ptr_via_local := issue26995_make_pointer_via_local[&int]()
	foo_ptr_via_local := issue26995_make_pointer_via_local[&Issue26995Foo]()
	assert *int_ptr == 0
	assert *foo_ptr == Issue26995Foo{}
	assert *int_ptr_via_local == 0
	assert *foo_ptr_via_local == Issue26995Foo{}
}

fn test_zero_new_typeof_payload_type_is_per_generic_instantiation() {
	assert issue26995_zero_payload_from_typeof[int](none) == 0
	assert issue26995_zero_payload_from_typeof[string](none) == ''
	assert *issue26995_new_payload_from_typeof[int](none) == 0
	assert *issue26995_new_payload_from_typeof[string](none) == ''
}

fn test_zero_field_payload_type_is_per_field() {
	mut x := Issue26996Options{}
	issue26996_fill(mut x)
	assert x.a? == 0
	assert x.b? == ''

	mut y := Issue26996OptionsReversed{}
	issue26996_fill(mut y)
	assert y.a? == ''
	assert y.b? == 0
}

fn test_inline_type_accessor_zero_init_can_cross_module_privacy() {
	opt := maybe_private_payload()
	comptime_accessor_helper.zero_option_payload(opt)
}
