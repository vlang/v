[heap]
struct MyHeapStruct {
	name string
}

// make sure dereferencing of heap stucts works in selector expr (in tmpl),
fn test_heap_struct_dereferencing_in_selector_expr() {
	a := MyHeapStruct{
		name: 'my_heap_struct_a'
	}
	b := 2
	$tmpl('comptime_call_tmpl_variable_scope_test.tpl')
}
