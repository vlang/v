module types

import v.flat

fn method_value_metadata_reader(tc &TypeChecker, receiver u32, start chan bool) {
	_ := <-start
	for _ in 0 .. 2000 {
		typ := tc.method_value_type(receiver, 'take') or { panic('missing method type') }
		assert typ is FnType
		if typ is FnType {
			assert typ.params.len == 1
			assert *typ.params[0].typ == Type(int_)
			assert typ.params[0].is_mut
			assert *typ.return_type == Type(string_)
			assert typ.is_variadic
		}
	}
}

fn test_method_value_queries_preserve_shared_declaration_metadata() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	receiver := flat.source_type_id('SharedReceiver')
	name := 'SharedReceiver.take'
	tc.fn_ret_types[name] = Type(string_)
	tc.fn_param_types[name] = [Type(int_), Type(int_)]
	tc.declaration_param_mutability[name] = [false, true]
	tc.fn_variadic[name] = true
	// Publish canonical scalar types before the workers begin their lookups.
	assert tc.method_value_type(receiver, 'take')? is FnType
	start := chan bool{cap: 4}
	mut readers := []thread{}
	for _ in 0 .. 4 {
		readers << spawn method_value_metadata_reader(&tc, receiver, start)
	}
	for _ in 0 .. 4 { start <- true }
	readers.wait()
	assert tc.fn_param_types[name].len == 2
	assert tc.declaration_param_mutability[name] == [false, true]
}
