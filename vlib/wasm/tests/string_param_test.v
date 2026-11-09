module main

import wasm
import os

fn read_uleb(data []u8, ip &int) u32 {
	mut i := *ip
	mut result := u32(0)
	mut shift := u32(0)
	for {
		b := u32(data[i])
		i++
		result |= (b & 0x7f) << shift
		if b & 0x80 == 0 { break }
		shift += 7
	}
	unsafe { *ip = i }
	return result
}

fn test_component_magic() {
	core_bytes := os.read_file('vlib/wasm/tests/testdata/guest.wasm') or { panic(err) }
	wrapped := wasm.wrap_core_module(core_bytes.bytes())
	assert wasm.is_component(wrapped)
	assert wrapped.len > core_bytes.len
}

fn test_core_exports_echo_len_cabi() {
	core_bytes := os.read_file('vlib/wasm/tests/testdata/guest.wasm') or { panic(err) }
	data := core_bytes.bytes()
	mut i := 8
	mut found_memory := false
	mut found_cabi := false
	for i < data.len {
		sid := data[i]
		i++
		mut cnt_i := i
		sz := read_uleb(data, &cnt_i)
		i = cnt_i
		end := i + int(sz)
		if sid == 7 {
			mut ci := i
			cnt := read_uleb(data, &ci)
			for _ in 0 .. cnt {
				nlen := int(read_uleb(data, &ci))
				name := data[ci..ci + nlen].bytestr()
				ci += nlen
				kind := data[ci]
				ci++
				idx := read_uleb(data, &ci)
				_ = idx
				if kind == 0 && name == 'echo_len_cabi' { found_cabi = true }
				if kind == 2 && name == 'memory' { found_memory = true }
			}
		}
		i = end
	}
	assert found_memory
	assert found_cabi
}

fn test_component_file_exists() {
	core_bytes := os.read_file('vlib/wasm/tests/testdata/guest.wasm') or { panic(err) }
	wrapped := wasm.wrap_core_module(core_bytes.bytes())
	os.write_file_array('vlib/wasm/tests/testdata/guest.component.wasm', wrapped) or { panic(err) }
	assert os.is_file('vlib/wasm/tests/testdata/guest.component.wasm')
}