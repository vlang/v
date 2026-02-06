import v.reflection

@[test_struct]
struct Test {
	m map[int]string @[test]
	n ?string        @[test2; test3]
}

enum Flags {
	foo
	bar
}

type MySum = f64 | int

type MyAlias = int

fn foo() ?string {
	return ''
}

fn bar() !string {
	return ''
}

fn baz() (int, f64, string) {
	return 1, 2.0, 'foo'
}

fn test_flag_option() {
	funcs := reflection.get_funcs().filter(it.name == 'foo')
	assert funcs[0].return_typ.has_flag(.option)
}

fn test_flag_result() {
	funcs := reflection.get_funcs().filter(it.name == 'bar')
	assert funcs[0].return_typ.has_flag(.result)
}

fn test_array_sym() {
	var := ['abc', 'def']
	typ := reflection.type_of(var)
	assert typ.sym.kind == .array
	assert typ.sym.language == .v
	assert typ.sym.methods.len > 0
	assert typ.sym.methods.any(it.name == 'join')
	assert typ.sym.name == '[]string'
	assert (typ.sym.info as reflection.Array).nr_dims == 1
	assert (typ.sym.info as reflection.Array).elem_type == typeof[string]().idx
}

fn test_sumtype_sym() {
	var := MySum(1)
	typ := reflection.type_of(var)
	assert typ.sym.kind == .sum_type
	assert (typ.sym.info as reflection.SumType).variants[0] == typeof[f64]().idx
	assert (typ.sym.info as reflection.SumType).variants[1] == typeof[int]().idx
}

fn test_alias_sym() {
	var := MyAlias(1)
	typ := reflection.type_of(var)
	assert typ.sym.kind == .alias
	assert typ.sym.language == .v
	assert (typ.sym.info as reflection.Alias).parent_idx == typeof[int]().idx
	assert typ.sym.methods.len == 0
}

fn test_multi_return_sym() {
	func := reflection.get_funcs().filter(it.name == 'baz')[0]
	assert func.name == 'baz'
	assert func.args.len == 0
	assert func.is_variadic == false
	assert func.return_typ.has_flag(.option) == false
	assert func.return_typ.has_flag(.result) == false
	assert func.return_typ.has_flag(.shared_f) == false
	assert func.receiver_typ == 0
	assert func.is_pub == false

	typ := reflection.get_type(int(func.return_typ))?
	assert typ.name == '(int, f64, string)'
	assert typ.sym.mod == ''
	assert typ.sym.language == .v
	assert typ.sym.kind == .multi_return
}

fn test_enum_sym() {
	var := reflection.type_of(Flags.foo)
	assert var.sym.name == 'Flags'
	assert var.sym.mod == 'main'
	assert var.sym.parent_idx == 0
	assert var.sym.kind == .enum
	assert var.sym.language == .v
	assert (var.sym.info as reflection.Enum).vals == ['foo', 'bar']
}

fn test_struct_sym() {
	var := reflection.type_of(Test{})
	assert var.sym.kind == .struct
	assert var.sym.mod == 'main'
	assert (var.sym.info as reflection.Struct).attrs.len == 1
	assert (var.sym.info as reflection.Struct).attrs == ['test_struct']

	field := (var.sym.info as reflection.Struct).fields[0]
	field_typ := field.typ
	field_sym := reflection.get_type(field_typ.idx())?
	assert field_sym.name == 'map[int]string'
	assert field_sym.sym.kind == .map
	assert (field_sym.sym.info as reflection.Map).key_type.idx() == typeof[int]().idx
	assert (field_sym.sym.info as reflection.Map).value_type.idx() == typeof[string]().idx
	assert field.attrs.len == 1

	field2 := (var.sym.info as reflection.Struct).fields[1]
	field2_typ := (var.sym.info as reflection.Struct).fields[1].typ
	assert field2_typ.has_flag(.option)
	assert field2.name == 'n'
	assert field2.attrs.len == 2
	assert field2.attrs == ['test2', 'test3']
	assert field2.is_pub == false
}
