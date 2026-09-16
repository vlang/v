import os

const vinix_selector_vexe = @VEXE
const vinix_selector_test_dir = os.join_path(os.dir(@FILE), 'testdata', 'v3_linker_symbol_cast')
const vinix_codegen_edges_test_dir = os.join_path(os.dir(@FILE), 'testdata',
	'v3_vinix_codegen_edges')

fn test_v3_preserves_vinix_style_selectors_and_linker_symbol_casts() {
	root := os.join_path(os.temp_dir(), 'v3_vinix_selectors_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	out := os.join_path(root, 'out.c')
	input := os.join_path(vinix_selector_test_dir, 'main.v')
	result :=
		os.execute('${os.quoted_path(vinix_selector_vexe)} -new-compiler -nocache -os vinix -target-libc-headers -enable-globals -no-closures -o ${os.quoted_path(out)} ${os.quoted_path(input)}')
	assert result.exit_code == 0, result.output
	code := os.read_file(out) or { panic(err) }
	assert code.contains('void main__kernel_entry(void)'), code
	assert code.contains('use_pointer(name.str);'), code
	assert !code.contains('closure__'), code
	assert code.contains('u64 address = (u64)((void*)(linker_symbol));'), code
	assert code.contains('u64 text_phys = (address - virtual_base) + physical_base;'), code
	assert code.contains('device__initialise();'), code
	assert !code.contains('device__Device__initialise'), code
}

fn test_v3_preserves_vinix_kernel_codegen_edges() {
	root := os.join_path(os.temp_dir(), 'v3_vinix_codegen_edges_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	out := os.join_path(root, 'out.c')
	input := os.join_path(vinix_codegen_edges_test_dir, 'main.v')
	result :=
		os.execute('${os.quoted_path(vinix_selector_vexe)} -new-compiler -nocache -os vinix -arch arm64 -target-libc-headers -enable-globals -no-closures -nofloat -o ${os.quoted_path(out)} ${os.quoted_path(input)}')
	assert result.exit_code == 0, result.output
	code := os.read_file(out) or { panic(err) }
	assert code.contains('u32 __order_snapshot_0 = value;'), code
	assert code.contains('char* __order_snapshot_1 = (char*)(name.str);'), code
	assert code.contains('return consume_order(__order_snapshot_0, __order_snapshot_1, __order_snapshot_2,'),
		code
	assert code.contains('u64 old = (*(*entry));'), code
	assert !code.contains('u64* old ='), code
	assert code.contains('main__Segment_is_writable(&segments[0])'), code
	assert code.contains('power__Segment__is_writable(&segments[0])'), code
	assert code.contains('v_free(pointer);'), code
	assert !code.contains('main__free('), code
	assert !code.contains('inline bool f32__eq_epsilon('), code
	assert !code.contains('inline string f32__str('), code
	assert !code.contains('inline string f64__str('), code
}
