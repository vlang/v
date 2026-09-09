@[heap]
struct MyHeapStruct {
	name string
}

// make sure dereferencing of heap structs works in selector expr (in tmpl),
fn test_heap_struct_dereferencing_in_selector_expr() {
	a := MyHeapStruct{
		name: 'my_heap_struct_a'
	}
	b := 2
	out := $tmpl('comptime_call_tmpl_variable_scope_test.tpl')
	assert out.trim_space() == 'my_heap_struct_a\n2'
}
