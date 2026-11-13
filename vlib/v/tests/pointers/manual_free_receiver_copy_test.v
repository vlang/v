module main

struct ManualFreeObservation {
mut:
	receiver voidptr
	value    int
}

struct ManualFreeValue {
	observation &ManualFreeObservation
mut:
	value int
}

@[unsafe]
fn (mut value ManualFreeValue) free() {
	value.observation.receiver = value
	value.observation.value = value.value
	value.value = -1
	unsafe { free(voidptr(value)) }
}

fn test_manual_free_copies_value_receiver() {
	mut observation := ManualFreeObservation{}
	mut value := ManualFreeValue{ observation: &observation, value: 42 }
	unsafe { value.free() }
	assert observation.value == 42
	assert value.value == 42
}

fn test_manual_free_keeps_heap_receiver() {
	mut observation := ManualFreeObservation{}
	mut value := &ManualFreeValue{ observation: &observation, value: 73 }
	expected := voidptr(value)
	unsafe { value.free() }
	assert observation.value == 73
	assert observation.receiver == expected
}

@[heap]
struct ManualFreeHeapValue {
	observation &ManualFreeObservation
	value       int
}

@[unsafe]
fn (mut value ManualFreeHeapValue) free() {
	value.observation.receiver = value
	value.observation.value = value.value
	unsafe { free(value) }
}

fn test_manual_free_keeps_heap_promoted_value() {
	mut observation := ManualFreeObservation{}
	mut value := ManualFreeHeapValue{ observation: &observation, value: 83 }
	expected := voidptr(&value)
	unsafe { value.free() }
	assert observation.value == 83
	assert observation.receiver == expected
}

fn release_manual_free_mut_receiver(mut value ManualFreeValue) {
	unsafe { value.free() }
}

fn test_manual_free_keeps_mut_receiver_storage() {
	mut observation := ManualFreeObservation{}
	mut value := &ManualFreeValue{ observation: &observation, value: 91 }
	expected := voidptr(value)
	release_manual_free_mut_receiver(mut value)
	assert observation.value == 91
	assert observation.receiver == expected
}

struct ManualFreeChild {
	child &u8
mut:
	cleaned bool
}

@[unsafe]
fn (mut value ManualFreeChild) free() {
	unsafe { free(value.child) }
	value.cleaned = true
}

fn test_manual_free_child_cleanup_keeps_value_receiver() {
	mut value := ManualFreeChild{ child: unsafe { malloc(1) } }
	unsafe { value.free() }
	assert value.cleaned
}

struct ManualFreeNested {
mut:
	cleaned bool
}

fn (mut value ManualFreeNested) free() {
	callback := fn (value &ManualFreeNested) {
		unsafe { free(value) }
	}
	_ = callback
	value.cleaned = true
}

fn test_manual_free_nested_function_does_not_release_receiver() {
	mut value := ManualFreeNested{}
	value.free()
	assert value.cleaned
}
