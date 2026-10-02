import os
import v.cmdexec

fn test_flat_payload_table_atomic_slot_has_a_c_compatible_pointer_type() {
	root := os.join_path(os.vtmp_dir(), 'flat_payload_pointer_abi_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	saved_fallback := os.getenv('V_MACOS_V3_NO_FALLBACK')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	defer { os.setenv('V_MACOS_V3_NO_FALLBACK', saved_fallback, true) }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import v.flat
fn main() {
 mut ids := []u32{}
 for i in 0 .. 2048 { ids << flat.node_payload(["T\${i}", "U"]) }
 for i, id in ids { assert flat.node_payload_at(id).generic_params == ["T\${i}", "U"] }
}
')!
	// Cover both integer atomic widths even when the host only runs one of them.
	for arch, width in {
		'i386':  '32'
		'amd64': '64'
	} {
		c_file := os.join_path(root, 'payload_${width}.c')
		generated := cmdexec.run(@VEXE, ['-gc', 'none', '-arch', arch, '-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(c_file)!
		assert code.contains('atomic_load_u${width}((void*)(&flat__g_node_payload_table))')
		assert code.contains('atomic_store_u${width}((void*)(&flat__g_node_payload_table),')
	}
	mut args := ['-gc', 'none', '-cc', @CCOMPILER]
	$if clang || gcc {
		// Clang 16+ rejects the historical NodePayloadTable** -> uint64_t* ABI.
		args << ['-cstrict', '-cflags', '-Werror=incompatible-pointer-types']
	}
	$if clang {
		// Keep existing const-discard warnings separate from the slot ABI mismatch.
		args << ['-cflags', '-Wno-error=incompatible-pointer-types-discards-qualifiers']
	}
	args << ['run', source]
	result := cmdexec.run(@VEXE, args)
	assert result.exit_code == 0, result.output
}
