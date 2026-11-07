module multi_return_models

#include "@VMODROOT/vlib/v/tests/options/generic_result_type_identity/multi_return_models/opaque_handle.h"

fn C.result_identity_get_handle() &C.multi_return_handle_t
fn C.result_identity_handle_value(&C.multi_return_handle_t) int

// optional_handle returns a header-only C type without a V redeclaration.
pub fn optional_handle(present bool) ?&C.multi_return_handle_t {
	if !present {
		return none
	}
	return C.result_identity_get_handle()
}

// result_handle returns a header-only C type through a result.
pub fn result_handle() !&C.multi_return_handle_t {
	return C.result_identity_get_handle()
}

// handle_value reads the opaque handle through its C API.
pub fn handle_value(handle &C.multi_return_handle_t) int {
	return C.result_identity_handle_value(handle)
}
