module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_payload_table_preserves_constant_array_layout_across_chunks() {
	$if macos && arm64 {
		flat_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'flat', 'flat.v')) or {
			panic(err)
		}
		allocator_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'flat',
			'flat_payload.c.v')) or { panic(err) }
		constants := flat_source.all_after('const node_payload_chunk_bits =').all_before('// canonical_comptime_type_payload')
		table := flat_source.all_after('struct NodePayloadTable {').all_before('__global g_node_payload_table')
		new_table := allocator_source.all_after('fn node_payload_new_table()').all_before('// node_payload_new_chunk')
		new_chunk := allocator_source.all_after('fn node_payload_new_chunk()')
		payload_source := 'module flat\nfn C.calloc(usize, usize) voidptr\nfn C.free(voidptr)\nfn C.v_flat_payload_ptr_get(voidptr, usize) voidptr\nfn C.v_flat_payload_ptr_set(voidptr, usize, voidptr)\nconst node_payload_chunk_bits =' +
			constants + 'struct NodePayloadTable {' + table +
			'fn node_payload_alloc_zeroed(n usize) voidptr { return unsafe { C.calloc(1, n) } }\nfn node_payload_new_table()' +
			new_table + 'fn node_payload_new_chunk()' + new_chunk + r'
pub fn verify() int {
    if sizeof(NodePayloadTable) != 32776 { return 1 }
    mut table := node_payload_new_table()
    defer { unsafe { C.free(table) } }
    mut payloads := [4097]int{}
    for index in 0 .. 4097 {
        payloads[index] = index + 1
        chunk_index := int(table.count) >> node_payload_chunk_bits
        mut chunk := C.v_flat_payload_ptr_get(voidptr(table), usize(chunk_index))
        if chunk == voidptr(0) {
            chunk = node_payload_new_chunk()
            C.v_flat_payload_ptr_set(voidptr(table), usize(chunk_index), chunk)
        }
        C.v_flat_payload_ptr_set(chunk, usize(int(table.count) & node_payload_chunk_mask),
            unsafe { voidptr(&payloads[index]) })
        table.count += 1
    }
    first := C.v_flat_payload_ptr_get(voidptr(table), 0)
    second := C.v_flat_payload_ptr_get(voidptr(table), 1)
    defer {
        unsafe {
            C.free(first)
            C.free(second)
        }
    }
    if first == second || second == voidptr(0) || table.count != 4097 { return 2 }
    if C.v_flat_payload_ptr_get(first, 0) != unsafe { voidptr(&payloads[0]) } { return 3 }
    if C.v_flat_payload_ptr_get(first, 4095) != unsafe { voidptr(&payloads[4095]) } { return 4 }
    if C.v_flat_payload_ptr_get(second, 0) != unsafe { voidptr(&payloads[4096]) } { return 5 }
    if payloads[0] != 1 || payloads[4095] != 4096 || payloads[4096] != 4097 { return 6 }
    return 0
}
'
		source := 'module main\nimport flat\nfn C.exit(int)\nfn C.alarm(u32) u32\nconst node_payload_max_chunks = 2\nfn main() {\n C.alarm(10)\n C.exit(flat.verify())\n}\n'
		path := os.join_path(os.vtmp_dir(), 'arm64_payload_table_${os.getpid()}.v')
		output := path.all_before_last('.')
		payload_path := output + '_flat.v'
		defer {
			os.rm(path) or {}
			os.rm(payload_path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, source) or { panic(err) }
		os.write_file(payload_path, payload_source) or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_files([payload_path, path])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
