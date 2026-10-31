import v.tests.generics.generics_from_modules.genericmodule as gm

struct Box[T] {
	values []T
}

type ReceiverAlias = int

fn test_imported_generic_receiver_infers_its_declaring_module() {
	local := Box[int]{ values: [7] }
	assert local.values == [7]
	first := gm.box(42)
	second := gm.box(42)
	assert first.get() == 42
	assert first.same(second)
	assert gm.read_box(first) == 42
	copy := first.copy()!
	assert copy.get() == 42
}

fn test_imported_generic_receiver_keeps_distinct_specializations() {
	first := gm.box(f32(1.5))
	second := gm.box(f64(2.5))
	assert typeof(first.get()).name == 'f32'
	assert typeof(second.get()).name == 'f64'
	assert first.get() == f32(1.5)
	assert second.get() == f64(2.5)
	assert gm.read_box(first) == f32(1.5)
	assert gm.read_box(second) == f64(2.5)
}

fn test_imported_generic_receiver_typeof_preserves_alias() {
	box := gm.box(ReceiverAlias(9))
	assert typeof(box.get()).name == 'ReceiverAlias'
	assert typeof[ReceiverAlias]().name == 'ReceiverAlias'
}

fn test_generic_receiver_inference_after_result_propagation() ! {
	assert gm.result_box(f32(1.5))!.get() == f32(1.5)
	assert gm.result_box(f64(2.5))!.plus(3) == f64(5.5)
	assert typeof(gm.result_box(f32(1.5))!.get()).name == 'f32'
	assert typeof(gm.result_box(f64(2.5))!.plus(3)).name == 'f64'
}

fn test_generic_receiver_inference_after_option_unwrap() {
	assert (gm.optional_box(f32(1.5)) or { gm.box(f32(0)) }).get() == f32(1.5)
	assert (gm.box(f64(2.5))).plus(3) == f64(5.5)
}
