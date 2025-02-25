// ... (previous functions: u32, patch_start, etc.)
// Copyright (c) 2023 l-m.dev. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// wasm module encoding implements serialization of WebAssembly modules into the binary format.
// This file handles the encoding logic for all sections defined in the spec.
module wasm

import encoding.leb128
import math.bits

// u32 encodes an unsigned 32-bit integer into the module’s buffer using LEB128.
fn (mut mod Module) u32(v u32) {
	mod.buf << leb128.encode_u32(v)
}

// patch_start returns the current position in the buffer for later patching.
fn (mut mod Module) patch_start() int {
	return mod.buf.len
}

// patch_len inserts the length of a section or subsection at the given position.
fn (mut mod Module) patch_len(pos int) {
	len := mod.buf.len - pos
	data := leb128.encode_u32(u32(len))
	mod.buf.insert(pos, data)
}

// patch_u32 inserts a specific u32 value at the given position.
fn (mut mod Module) patch_u32(pos int, val u32) {
	data := leb128.encode_u32(val)
	mod.buf.insert(pos, data)
}

// result_type encodes a vector of result types (parameters or returns).
fn (mut mod Module) result_type(results []ValType) {
	mod.u32(u32(results.len))
	for r in results {
		mod.buf << u8(r)
	}
}

// function_type encodes a function type (parameters and results).
fn (mut mod Module) function_type(ft FuncType) {
	mod.buf << 0x60 // Function type indicator
	mod.result_type(ft.parameters)
	mod.result_type(ft.results)
}

// global_type encodes a global type with mutability flag.
fn (mut mod Module) global_type(vt ValType, is_mut bool) {
	mod.buf << u8(vt)
	mod.buf << u8(is_mut)
}

// push_f32 encodes a 32-bit float into the buffer.
fn push_f32(mut buf []u8, v f32) {
	rv := bits.f32_bits(v)
	buf << u8(rv >> u32(0))
	buf << u8(rv >> u32(8))
	buf << u8(rv >> u32(16))
	buf << u8(rv >> u32(24))
}

// push_f64 encodes a 64-bit float into the buffer.
fn push_f64(mut buf []u8, v f64) {
	rv := bits.f64_bits(v)
	buf << u8(rv >> u32(0))
	buf << u8(rv >> u32(8))
	buf << u8(rv >> u32(16))
	buf << u8(rv >> u32(24))
	buf << u8(rv >> u32(32))
	buf << u8(rv >> u32(40))
	buf << u8(rv >> u32(48))
	buf << u8(rv >> u32(56))
}

// get_function_idx resolves the index of a function (local or imported) for patching.
fn (mod &Module) get_function_idx(patch CallPatch) int {
	mut idx := -1
	match patch {
		FunctionCallPatch {
			ftt := mod.functions[patch.name] or {
				panic('Called function "${patch.name}" does not exist')
			}
			idx = ftt.idx + mod.fn_imports.len
		}
		ImportCallPatch {
			for fnidx, c in mod.fn_imports {
				if c.mod == patch.mod && c.name == patch.name {
					idx = fnidx
					break
				}
			}
			if idx == -1 {
				panic('Called imported function "${patch.mod}.${patch.name}" does not exist')
			}
		}
	}
	return idx
}

// patch applies function patches (calls or globals) to the code buffer.
fn (mut mod Module) patch(ft Function) {
	mut ptr := 0
	for patch in ft.patches {
		mut idx := 0
		match patch {
			CallPatch {
				idx = mod.get_function_idx(patch)
			}
			FunctionGlobalPatch {
				idx = mod.global_imports.len + patch.idx
			}
		}
		mod.buf << ft.code[ptr..patch.pos]
		mod.u32(u32(idx))
		ptr = patch.pos
	}
	mod.buf << ft.code[ptr..]
}

// name encodes a string as a name (length-prefixed byte array).
fn (mut mod Module) name(name string) {
	mod.u32(u32(name.len))
	mod.buf << name.bytes()
}

// start_subsection begins a custom subsection and returns its patch position.
fn (mut mod Module) start_subsection(sec Subsection) int {
	mod.buf << u8(sec)
	return mod.patch_start()
}

// start_section begins a section and returns its patch position.
fn (mut mod Module) start_section(sec Section) int {
	mod.buf << u8(sec)
	return mod.patch_start()
}

// end_section patches the length of a section or subsection.
fn (mut mod Module) end_section(tpatch int) {
	mod.patch_len(tpatch)
}

// compile serializes the WebAssembly module into a byte array.
// Returns a binary that can be written to a .wasm file or executed in memory.
// Implements the full WebAssembly binary format: https://webassembly.github.io/spec/core/binary/modules.html
pub fn (mut mod Module) compile() []u8 {
	mod.buf = []u8{cap: 128}

	// WASM_BINARY_MAGIC, WASM_BINARY_VERSION
	mod.buf << [u8(0x00), 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]

	// Type Section
	// https://webassembly.github.io/spec/core/binary/modules.html#type-section
	if mod.functypes.len > 0 {
		tpatch := mod.start_section(.type_section)
		{
			mod.u32(u32(mod.functypes.len))
			for ft in mod.functypes {
				mod.function_type(ft)
			}
		}
		mod.end_section(tpatch)
	}

	// Import Section
	// https://webassembly.github.io/spec/core/binary/modules.html#import-section
	if mod.fn_imports.len > 0 || mod.global_imports.len > 0 {
		tpatch := mod.start_section(.import_section)
		{
			mod.u32(u32(mod.fn_imports.len + mod.global_imports.len))
			for ft in mod.fn_imports {
				mod.name(ft.mod)
				mod.name(ft.name)
				mod.buf << 0x00 // Function kind
				mod.u32(u32(ft.tidx))
			}
			for gt in mod.global_imports {
				mod.name(gt.mod)
				mod.name(gt.name)
				mod.buf << 0x03 // Global kind
				mod.global_type(gt.typ, gt.is_mut)
			}
		}
		mod.end_section(tpatch)
	}

	// Function Section
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec
	if mod.functions.len > 0 {
		tpatch := mod.start_section(.function_section)
		{
			mod.u32(u32(mod.functions.len))
			for _, ft in mod.functions {
				mod.u32(u32(ft.tidx))
			}
		}
		mod.end_section(tpatch)
	}

	// Memory Section
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-memsec
	if memory := mod.memory {
		tpatch := mod.start_section(.memory_section)
		{
			mod.u32(1)
			if max := memory.max {
				mod.buf << 0x01 // limit, max present
				mod.u32(memory.min)
				mod.u32(max)
			} else {
				mod.buf << 0x00 // limit, max not present
				mod.u32(memory.min)
			}
		}
		mod.end_section(tpatch)
	}

	// Global Section
	// https://webassembly.github.io/spec/core/binary/modules.html#global-section
	if mod.globals.len > 0 {
		tpatch := mod.start_section(.global_section)
		{
			mod.u32(u32(mod.globals.len))
			for gt in mod.globals {
				mod.global_type(gt.typ, gt.is_mut)

				{
					mut ptr := 0
					for patch in gt.init.call_patches {
						idx := mod.get_function_idx(patch)

						mod.buf << gt.init.code[ptr..patch.pos]
						mod.u32(u32(idx))
						ptr = patch.pos
					}
					mod.buf << gt.init.code[ptr..]
				}
				mod.buf << 0x0B // END expression opcode
			}
		}
		mod.end_section(tpatch)
	}

	// Export Section
	// https://webassembly.github.io/spec/core/binary/modules.html#export-section
	{
		tpatch := mod.start_section(.export_section)
		{
			lpatch := mod.patch_start()
			mut lsz := 0
			for _, ft in mod.functions {
				if !ft.export {
					continue
				}
				lsz++
				mod.name(ft.export_name or { ft.name })
				mod.buf << 0x00 // function
				mod.u32(u32(ft.idx + mod.fn_imports.len))
			}
			if memory := mod.memory {
				if memory.export {
					lsz++
					mod.name(memory.name)
					mod.buf << 0x02 // function
					mod.u32(0)
				}
			}
			for idx, gbl in mod.globals {
				if !gbl.export {
					continue
				}
				lsz++
				mod.name(gbl.name)
				mod.buf << 0x03 // global
				mod.u32(u32(idx + mod.global_imports.len))
			}
			mod.patch_u32(lpatch, u32(lsz))
		}
		mod.end_section(tpatch)
	}

	// Start Section
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-startsec
	if start := mod.start {
		ftt := mod.functions[start] or { panic('Start function "${start}" does not exist') }
		tpatch := mod.start_section(.start_section)
		{
			mod.u32(u32(ftt.idx + mod.fn_imports.len))
		}
		mod.end_section(tpatch)
	}

	// Data Count Section
	// https://webassembly.github.io/spec/core/binary/modules.html#data-count-section
	if mod.segments.len > 0 {
		tpatch := mod.start_section(.data_count_section)
		{
			mod.u32(u32(mod.segments.len))
		}
		mod.end_section(tpatch)
	}

	// Code Section
	// https://webassembly.github.io/spec/core/binary/modules.html#binary-codesec

	if mod.functions.len > 0 {
		tpatch := mod.start_section(.code_section)
		{
			mod.u32(u32(mod.functions.len))
			for _, ft in mod.functions {
				fpatch := mod.patch_start()
				rloc := ft.locals[mod.functypes[ft.tidx].parameters.len..]
				{
					mod.u32(u32(rloc.len))
					for lt in rloc {
						mod.u32(1)
						mod.buf << u8(lt.typ)
					}
					mod.patch(ft)
					mod.buf << 0x0B // END expression opcode
				}
				mod.patch_len(fpatch)
			}
		}
		mod.end_section(tpatch)
	}

	// Data Section
	// https://webassembly.github.io/spec/core/binary/modules.html#data-section
	if mod.segments.len > 0 {
		tpatch := mod.start_section(.data_section)
		{
			mod.u32(u32(mod.segments.len))
			for _, seg in mod.segments {
				if idx := seg.idx {
					mod.buf << 0x00 // Active segment
					// constant expr
					mod.buf << 0x41 // i32.const
					mod.buf << leb128.encode_i32(i32(idx))
					mod.buf << 0x0B // END opcode
				} else {
					mod.buf << 0x01 // Passive segment
				}
				mod.u32(u32(seg.data.len))
				mod.buf << seg.data
			}
		}
		mod.end_section(tpatch)
	}

	// Custom Section (Debug Info)
	// https://webassembly.github.io/spec/core/appendix/custom.html#name-section
	if mod.debug {
		tpatch := mod.start_section(.custom_section)
		mod.name('name')
		if mod_name := mod.mod_name {
			mpatch := mod.start_subsection(.name_module)
			{
				mod.name(mod_name)
			}
			mod.end_section(mpatch)
		}
		{
			mpatch := mod.start_subsection(.name_function)
			{
				mod.u32(u32(mod.functions.len + mod.fn_imports.len))
				mut idx := 0
				for f in mod.fn_imports {
					mod.u32(u32(idx))
					mod.name('${f.mod}.${f.name}')
					idx++
				}
				for n, _ in mod.functions {
					mod.u32(u32(idx))
					mod.name(n)
					idx++
				}
			}
			mod.end_section(mpatch)
		}
		{
			mpatch := mod.start_subsection(.name_local)
			{
				fpatch := mod.patch_start()
				mut fcount := 0
				mut idx := mod.fn_imports.len // after imports
				for _, ft in mod.functions {
					// only add entry if it contains a local with an assigned name
					if ft.locals.any(it.name != none) {
						mod.u32(u32(idx)) // function idx

						mut lcount := 0
						lcpatch := mod.patch_start()
						for lidx, loc in ft.locals {
							if name := loc.name {
								mod.u32(u32(lidx))
								mod.name(name)
								lcount++
							}
						}
						mod.patch_u32(lcpatch, u32(lcount))
						fcount++
					}
					idx++
				}
				mod.patch_u32(fpatch, u32(fcount))
			}
			mod.end_section(mpatch)
		}
		{
			mpatch := mod.start_subsection(.name_type)
			{
				fpatch := mod.patch_start()
				mut fcount := 0
				for idx, ft in mod.functypes {
					if name := ft.name {
						mod.u32(u32(idx))
						mod.name(name)
						fcount++
					}
				}
				mod.patch_u32(fpatch, u32(fcount))
			}
			mod.end_section(mpatch)
		}
		if memory := mod.memory {
			mpatch := mod.start_subsection(.name_memory)
			{
				mod.u32(u32(1)) // one memory in vec
				mod.u32(u32(0)) // 0 idx
				mod.name(memory.name) // memory name
			}
			mod.end_section(mpatch)
		}
		if mod.globals.len != 0 || mod.global_imports.len != 0 {
			mpatch := mod.start_subsection(.name_global)
			{
				fpatch := mod.patch_start()
				mut fcount := 0
				for gbl in mod.global_imports {
					mod.u32(u32(fcount))
					mod.name('${gbl.mod}.${gbl.name}')
					fcount++
				}
				for gbl in mod.globals {
					mod.u32(u32(fcount))
					mod.name(gbl.name)
					fcount++
				}
				mod.patch_u32(fpatch, u32(fcount))
			}
			mod.end_section(mpatch)
		}
		if mod.segments.any(it.name != none) {
			mpatch := mod.start_subsection(.name_data)
			{
				fpatch := mod.patch_start()
				mut fcount := 0
				for idx, ds in mod.segments {
					if name := ds.name {
						mod.u32(u32(idx))
						mod.name(name)
						fcount++
					}
				}
				mod.patch_u32(fpatch, u32(fcount))
			}
			mod.end_section(mpatch)
		}

		mod.end_section(tpatch)
	}

	return mod.buf
}
