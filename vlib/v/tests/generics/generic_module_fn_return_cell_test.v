import fn_return_cell_mod
import fn_return_cell_client

fn test_generic_callback_return_cell_in_imported_module() {
	assert fn_return_cell_mod.empty[int]()
	assert fn_return_cell_mod.empty[string]()
	assert fn_return_cell_mod.invoke[int](fn () int {
		return 42
	}) == 42
	assert fn_return_cell_mod.invoke[string](fn () string {
		return 'hello'
	}) == 'hello'
}

fn test_generic_callback_parameter_cell_in_imported_module() {
	assert fn_return_cell_mod.apply[int](fn (value int) int {
		return value + 1
	}, 10) == 11
	assert fn_return_cell_mod.apply[string](fn (value string) int {
		return value.len
	}, 'hello') == 5
}

fn test_generic_callback_cell_declared_in_a_third_module() {
	assert fn_return_cell_client.empty[int]()
	assert fn_return_cell_client.empty[string]()
}
