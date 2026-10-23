module c

import os
import v.cmdexec

fn test_unreferenced_c_struct_binding_does_not_redefine_header_type() ! {
	root := os.join_path(os.vtmp_dir(), 'unreferenced_c_struct_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'import net as _
struct C.pollfd {
mut:
 fd i32
 events i16
 revents i16
}
struct C.UnusedInner { value int }
struct C.UnusedOuter { items [1]C.UnusedInner }
struct C.AliasOnly { value int }
type AliasOnlyArray = [1]C.AliasOnly
struct C.UsedInner { value int }
struct C.UsedOuter { items [1]C.UsedInner }
struct C.KeptLayout { value int }
struct C.DirectLayout { value int }
struct C.SizeOnlyLayout { value i64 }
struct C.OffsetOnlyLayout { prefix u8 value i64 }
struct Holder { items [1]C.KeptLayout }
fn main() {
 assert sizeof(C.SizeOnlyLayout) == sizeof(i64)
 assert __offsetof(C.OffsetOnlyLayout, value) >= 1
 direct := C.DirectLayout{7}
 assert direct.value == 7
 used := C.UsedOuter{}
 assert used.items[0].value == 0
 holder := Holder{}
 assert holder.items[0].value == 0
}')!
	generated_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-o', output, source])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(output)!
	assert !generated.contains('struct pollfd {')
	assert !generated.contains('UnusedInner')
	assert !generated.contains('UnusedOuter')
	assert generated.contains('struct AliasOnly {')
	assert generated.contains('struct UsedInner {')
	assert generated.contains('struct UsedOuter {')
	assert generated.contains('struct KeptLayout {')
	assert generated.contains('struct DirectLayout {')
	assert generated.contains('struct SizeOnlyLayout {')
	assert generated.contains('struct OffsetOnlyLayout {')
	run_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', 'run', source])
	assert run_result.exit_code == 0, run_result.output
}

fn check_auxiliary_c_struct_declarations(name string, declarations string, retained []string, omitted []string) ! {
	root := os.join_path(os.vtmp_dir(), 'auxiliary_c_struct_${name}_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, declarations + '\nfn main() {}\n')!
	generated_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-enable-globals', '-o',
		output, source])
	assert generated_result.exit_code == 0, generated_result.output
	run_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-enable-globals', '-cc', 'clang',
		'run', source])
	assert run_result.exit_code == 0, run_result.output
	generated := os.read_file(output)!
	for name_to_keep in retained {
		assert generated.contains('struct ${name_to_keep} {')
	}
	for name_to_omit in omitted {
		assert !generated.contains(name_to_omit)
	}
}

fn test_unused_optional_aliases_keep_c_payloads() ! {
	check_auxiliary_c_struct_declarations('aliases', '
struct C.OptionalItem { value int }
struct C.UnusedAliasItem { value int }
type MaybeItem = ?C.OptionalItem
type UnusedAlias = C.UnusedAliasItem
', ['OptionalItem'], ['UnusedAliasItem'])!
}

fn test_unused_c_struct_callbacks_do_not_emit_signatures() ! {
	check_auxiliary_c_struct_declarations('callbacks', '
struct C.CallbackItem { value int }
struct C.UnusedCallbacks { cb fn (&C.CallbackItem) }
', [], ['CallbackItem', 'UnusedCallbacks'])!
}

fn test_unused_c_global_fixed_array_keeps_element_layout() ! {
	check_auxiliary_c_struct_declarations('globals', '
struct C.GlobalItem { value int }
__global C.items [1]C.GlobalItem
', ['GlobalItem'], [])!
}

fn test_empty_interface_stringification_omits_unused_c_layouts() ! {
	root := os.join_path(os.vtmp_dir(), 'interface_c_struct_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'struct C.UnusedInterfaceLayout { value int }
struct C.BoxedInterfaceLayout { value int }
interface Empty {}
struct Plain { value int }
fn main() {
	plain := Empty(Plain{7})
	assert "\${plain}".contains("Plain")
	assert plain is Plain
	boxed := Empty(C.BoxedInterfaceLayout{11})
	assert "\${boxed}".contains("BoxedInterfaceLayout")
	assert boxed is C.BoxedInterfaceLayout
}')!
	generated_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-o', output, source])
	assert generated_result.exit_code == 0, generated_result.output
	generated := os.read_file(output)!
	assert !generated.contains('UnusedInterfaceLayout')
	assert generated.contains('struct BoxedInterfaceLayout {')
	run_result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-cc', 'clang', 'run', source])
	assert run_result.exit_code == 0, run_result.output
}
