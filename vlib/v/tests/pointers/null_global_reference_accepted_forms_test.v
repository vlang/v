@[has_globals]
module main

struct GlobalItem {
	id int
}

type GlobalHandle = voidptr

__global (
	global_null     &GlobalItem = unsafe { nil }
	global_cast     &GlobalItem = unsafe { voidptr(0) }
	global_handle   GlobalHandle = voidptr(0)
	global_optional ?&GlobalItem = none
	global_present  &GlobalItem = &GlobalItem{ id: 7 }
)

fn test_typed_global_references_preserve_explicit_unsafe_and_valid_initializers() {
	assert isnil(global_null)
	assert isnil(global_cast)
	assert global_handle == voidptr(0)
	assert global_optional == none
	assert global_present.id == 7
}
