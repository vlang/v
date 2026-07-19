// Reference printing follows Go's `%v` rule: a pointer to a scalar
// (int, float, bool, string, rune) prints as its address, while a pointer to a
// struct/array/map prints as `&` followed by the pointed-to value.
// See https://github.com/vlang/v/issues/23461 and #15405.

struct Point {
	x int
	y int
}

type BoolAlias = bool
type StringAlias = string
type IntAlias = int
type OptionalIntAlias = ?int
type IntPointerAlias = &int
type StringPointerAlias = &string
type CustomLabel = int
type CustomLabelPointer = &CustomLabel
type RefCustomLabel = int
type RefCustomLabelPointer = &RefCustomLabel
type CustomIntPointer = &int
type WrappedCustomIntPointer = CustomIntPointer
type NestedIntAlias = int
type NestedIntPointerAlias = &NestedIntAlias

fn (label CustomLabel) str() string {
	return 'Label(${int(label)})'
}

fn (label &RefCustomLabel) str() string {
	return 'RefLabel(${int(*label)})'
}

fn (p CustomIntPointer) str() string {
	_ = p
	return 'IntPointer'
}

fn generic_ref_string[T](value T) string {
	return '${value}'
}

fn option_alias_ref_string(value OptionalIntAlias) string {
	return '${&value}'
}

fn identity_string(value string) string {
	return value
}

fn test_scalar_reference_prints_as_address() {
	i := 5
	s := 'Hello'
	b := true
	f := 1.5
	r := `A`
	// `${&x}` for a scalar equals `ptr_str` - the raw address, no `&`, no value
	assert '${&i}' == ptr_str(&i)
	assert '${&s}' == ptr_str(&s)
	assert '${&b}' == ptr_str(&b)
	assert '${&f}' == ptr_str(&f)
	assert '${&r}' == ptr_str(&r)
	assert 'int=${&i}, float=${&f}' == 'int=${ptr_str(&i)}, float=${ptr_str(&f)}'
	sp := &s
	assert '${sp}' == ptr_str(sp)
}

fn test_generic_scalar_reference_prints_as_address() {
	i := 5
	u := u32(7)
	assert generic_ref_string(&i) == ptr_str(&i)
	assert generic_ref_string(&u) == ptr_str(&u)
}

fn test_named_scalar_pointer_alias_prints_as_address() {
	i := 42
	p := IntPointerAlias(&i)
	assert '${p}' == ptr_str(p)
	println(p)

	s := 'hi'
	sp := StringPointerAlias(&s)
	assert '${sp}' == ptr_str(sp)
	assert '${sp:s}' == '&hi'
}

fn test_nested_scalar_pointer_alias_prints_as_address() {
	i := NestedIntAlias(43)
	p := NestedIntPointerAlias(&i)
	assert '${p}' == ptr_str(p)
	assert 'pointer=${p}' == 'pointer=${ptr_str(p)}'
	println(p)
}

fn test_scalar_alias_reference_honors_custom_str() {
	label := CustomLabel(7)
	assert '${&label}' == '&Label(7)'
	println(&label)

	p := CustomLabelPointer(&label)
	assert '${p}' == '&Label(7)'
	println(p)
}

fn test_scalar_alias_reference_honors_custom_ptr_str() {
	label := RefCustomLabel(8)
	assert '${&label}' == '&RefLabel(8)'
	println(&label)

	p := RefCustomLabelPointer(&label)
	assert '${p}' == '&RefLabel(8)'
	println(p)
}

fn test_scalar_pointer_alias_honors_custom_str() {
	i := 9
	p := CustomIntPointer(&i)
	assert '${p}' == 'IntPointer'
	println(p)
}

fn test_wrapped_scalar_pointer_alias_honors_parent_custom_str() {
	i := 10
	p := WrappedCustomIntPointer(&i)
	assert '${p}' == 'IntPointer'
	println(p)
}

fn test_option_alias_reference_prints_as_option_value() {
	assert option_alias_ref_string(?int(42)) == '&Option(42)'
	assert option_alias_ref_string(?int(none)) == '&Option(none)'
}

fn test_shared_scalars_print_as_values() {
	shared s := 'x'
	shared b := false
	shared i := 42
	rlock s, b, i {
		assert '${s}' == 'x'
		assert '${b}' == 'false'
		assert '${i}' == '42'
	}
}

fn test_nested_scalar_reference_keeps_address_format() {
	i := 42
	expected := ptr_str(&i)
	assert '${'${&i}'}' == expected
	assert '${identity_string('${&i}')}' == expected
}

fn test_scalar_alias_reference_prints_as_address() {
	// a pointer to an alias of a scalar keeps the scalar (address) behavior,
	// instead of being mis-stringified as its aliased value
	b := BoolAlias(false)
	pb := &b
	vpb := unsafe { voidptr(pb) }
	assert '${pb}' == '${vpb}'
	s := StringAlias('hi')
	ps := &s
	vps := unsafe { voidptr(ps) }
	assert '${ps}' == '${vps}'
	i := IntAlias(7)
	pi := &i
	vpi := unsafe { voidptr(pi) }
	assert '${pi}' == '${vpi}'
}

fn test_explicit_string_format_preserves_scalar_reference_values() {
	b := false
	pb := &b
	assert '${pb:s}' == '&false'
	s := 'hi'
	ps := &s
	assert '${ps:s}' == '&hi'

	ab := BoolAlias(false)
	pab := &ab
	assert '${pab:s}' == '&false'
	alias_string := StringAlias('hi')
	pas := &alias_string
	assert '${pas:s}' == '&hi'
}

fn test_compound_reference_prints_as_value() {
	p := Point{1, 2}
	arr := [1, 2, 3]
	m := {
		'a': 1
	}
	assert '${&p}' == '&${p}'
	assert '${&arr}' == '&${arr}'
	assert '${&m}' == '&${m}'
}
