import v.reflection

enum Flags {
	foo
	bar
}

type MySum = int | f64

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
        var := [1,2]
        typ := reflection.type_of(var)
        assert typ.sym.kind == .array
        assert typ.sym.language == .v
        assert typ.sym.methods.len > 0
        assert typ.sym.methods.filter(it.name == 'reduce').len > 0
}

fn test_sumtype_sym() {
        var := MySum(1)
	typ := reflection.type_of(var)
        assert typ.sym.kind == .sum_type
	assert (typ.sym.info as reflection.SumType).variants[0].is_int()
	assert (typ.sym.info as reflection.SumType).variants[1].is_float()
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

	typ := reflection.get_type(func.return_typ)?
	assert typ.name == '(int, f64, string)'
	assert typ.sym.language == .v
	assert typ.sym.kind == .multi_return
}

fn test_enum_sym() {
        var := reflection.type_of(Flags.foo)
	assert var.sym.name == 'main.Flags'
	assert var.sym.parent_idx == 0
	assert var.sym.kind == .enum_
	assert var.sym.language == .v
	assert (var.sym.info as reflection.Enum).vals == ['foo', 'bar']
}