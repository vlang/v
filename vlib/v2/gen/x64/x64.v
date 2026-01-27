// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import v2.ssa
import encoding.binary

pub struct Gen {
	mod &ssa.Module
mut:
	elf &ElfObject

	stack_map      map[int]int
	alloca_offsets map[int]int
	stack_size     int
	curr_offset    int

	block_offsets  map[int]int
	pending_labels map[int][]int

	// Register allocation
	reg_map   map[int]int
	used_regs []int
}

struct Interval {
mut:
	val_id   int
	start    int
	end      int
	has_call bool
}

pub fn Gen.new(mod &ssa.Module) &Gen {
	return &Gen{
		mod: mod
		elf: ElfObject.new()
	}
}

pub fn (mut g Gen) gen() {
	for func in g.mod.funcs {
		g.gen_func(func)
	}

	// Generate Globals in .data
	for gvar in g.mod.globals {
		for g.elf.data_data.len % 8 != 0 {
			g.elf.data_data << 0
		}
		addr := u64(g.elf.data_data.len)
		g.elf.add_symbol(gvar.name, addr, false, 2)
		if gvar.is_constant {
			// For constants, write the initial value
			mut bytes := []u8{len: 8}
			binary.little_endian_put_u64(mut bytes, u64(gvar.initial_value))
			for b in bytes {
				g.elf.data_data << b
			}
		} else {
			// For regular globals, initialize with zeros
			for _ in 0 .. 8 {
				g.elf.data_data << 0
			}
		}
	}
}

fn (mut g Gen) gen_func(func ssa.Function) {
	g.curr_offset = g.elf.text_data.len
	g.stack_map = map[int]int{}
	g.alloca_offsets = map[int]int{}
	g.block_offsets = map[int]int{}
	g.pending_labels = map[int][]int{}
	g.reg_map = map[int]int{}
	g.used_regs = []int{}

	g.allocate_registers(func)

	// Calculate Stack Frame
	mut slot_offset := 8

	for pid in func.params {
		g.stack_map[pid] = -slot_offset
		slot_offset += 8
	}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind != .instruction {
				continue
			}
			instr := g.mod.instrs[val.index]

			if instr.op == .alloca {
				// Calculate allocation size based on the type
				// The alloca result type is ptr(T), so get the element type
				ptr_type := g.mod.type_store.types[val.typ]
				elem_type := g.mod.type_store.types[ptr_type.elem_type]

				// Calculate size based on element type
				mut alloc_size := 64 // Default for non-array types
				if elem_type.kind == .array_t {
					// Get the array element type to determine element size
					arr_elem_type := g.mod.type_store.types[elem_type.elem_type]
					elem_size := if arr_elem_type.width > 0 {
						(arr_elem_type.width + 7) / 8 // bits to bytes, rounded up
					} else {
						8 // default to 64-bit
					}
					alloc_size = elem_type.len * elem_size
				}

				// Align to 16 bytes
				slot_offset = (slot_offset + 15) & ~0xF
				slot_offset += alloc_size
				g.alloca_offsets[val_id] = -slot_offset
				slot_offset += 8 // Slot for the pointer
			}

			if val_id in g.reg_map {
				continue
			}
			g.stack_map[val_id] = -slot_offset
			slot_offset += 8
		}
	}

	g.stack_size = (slot_offset + 16) & ~0xF

	g.elf.add_symbol(func.name, u64(g.curr_offset), true, 1)

	// Prologue
	// endbr64 - required for CET/IBT (Indirect Branch Tracking)
	g.emit(0xF3)
	g.emit(0x0F)
	g.emit(0x1E)
	g.emit(0xFA)
	g.emit(0x55) // push rbp
	g.emit(0x48)
	g.emit(0x89)
	g.emit(0xE5) // mov rbp, rsp

	// Push callee-saved regs
	// RBX(3), R12(12), R13(13), R14(14), R15(15)
	for r in g.used_regs {
		g.emit_push_reg(r)
	}

	// sub rsp, stack_size
	if g.stack_size > 0 {
		g.emit(0x48)
		if g.stack_size <= 127 {
			g.emit(0x83) // sub r/m64, imm8
			g.emit(0xEC)
			g.emit(u8(g.stack_size))
		} else {
			g.emit(0x81) // sub r/m64, imm32
			g.emit(0xEC)
			g.emit_u32(u32(g.stack_size))
		}
	}

	// Move Params (ABI: RDI, RSI, RDX, RCX, R8, R9)
	// First 6 args in registers, rest on stack at [rbp+16], [rbp+24], ...
	abi_regs := [7, 6, 2, 1, 8, 9]
	for i, pid in func.params {
		if i < 6 {
			src := abi_regs[i]
			if reg := g.reg_map[pid] {
				g.emit_mov_reg_reg(reg, src)
			} else {
				offset := g.stack_map[pid]
				g.emit_store_reg_mem(src, offset)
			}
		} else {
			// Stack parameters: [rbp+16] is 7th param, [rbp+24] is 8th, etc.
			stack_param_offset := 16 + (i - 6) * 8
			// Load from stack into RAX, then store to our slot
			g.emit_load_reg_mem(0, stack_param_offset)
			if reg := g.reg_map[pid] {
				g.emit_mov_reg_reg(reg, 0)
			} else {
				offset := g.stack_map[pid]
				g.emit_store_reg_mem(0, offset)
			}
		}
	}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		g.block_offsets[blk_id] = g.elf.text_data.len - g.curr_offset

		if offsets := g.pending_labels[blk_id] {
			for off in offsets {
				target := g.block_offsets[blk_id]
				rel := target - (off + 4)
				abs_off := g.curr_offset + off
				g.write_u32(abs_off, u32(rel))
			}
		}

		for val_id in blk.instrs {
			g.gen_instr(val_id)
		}
	}
}

fn (mut g Gen) gen_instr(val_id int) {
	instr := g.mod.instrs[g.mod.values[val_id].index]

	// Temps: 0=RAX, 1=RCX

	match instr.op {
		.add, .sub, .mul, .sdiv, .srem, .and_, .or_, .xor, .shl, .ashr, .lshr, .eq, .ne, .lt, .gt,
		.le, .ge {
			g.load_val_to_reg(0, instr.operands[0]) // RAX
			g.load_val_to_reg(1, instr.operands[1]) // RCX

			match instr.op {
				.add {
					g.emit(0x48)
					g.emit(0x01)
					g.emit(0xC8) // add rax, rcx
				}
				.sub {
					g.emit(0x48)
					g.emit(0x29)
					g.emit(0xC8) // sub rax, rcx
				}
				.mul {
					g.emit(0x48)
					g.emit(0x0F)
					g.emit(0xAF)
					g.emit(0xC1) // imul rax, rcx
				}
				.sdiv {
					// cqo: sign-extend rax to rdx:rax
					g.emit(0x48)
					g.emit(0x99)
					// idiv rcx
					g.emit(0x48)
					g.emit(0xF7)
					g.emit(0xF9)
				}
				.srem {
					// cqo: sign-extend rax to rdx:rax
					g.emit(0x48)
					g.emit(0x99)
					// idiv rcx (quotient in rax, remainder in rdx)
					g.emit(0x48)
					g.emit(0xF7)
					g.emit(0xF9)
					// mov rax, rdx (move remainder to rax)
					g.emit(0x48)
					g.emit(0x89)
					g.emit(0xD0)
				}
				.and_ {
					g.emit(0x48)
					g.emit(0x21)
					g.emit(0xC8) // and rax, rcx
				}
				.or_ {
					g.emit(0x48)
					g.emit(0x09)
					g.emit(0xC8) // or rax, rcx
				}
				.xor {
					g.emit(0x48)
					g.emit(0x31)
					g.emit(0xC8) // xor rax, rcx
				}
				.shl {
					// shl rax, cl (shift by low 8 bits of rcx)
					g.emit(0x48)
					g.emit(0xD3)
					g.emit(0xE0)
				}
				.ashr {
					// sar rax, cl (arithmetic shift right)
					g.emit(0x48)
					g.emit(0xD3)
					g.emit(0xF8)
				}
				.lshr {
					// shr rax, cl (logical shift right)
					g.emit(0x48)
					g.emit(0xD3)
					g.emit(0xE8)
				}
				.eq, .ne, .lt, .gt, .le, .ge {
					g.emit(0x48)
					g.emit(0x39)
					g.emit(0xC8) // cmp rax, rcx
					code := match instr.op {
						.eq { 0x94 }
						.ne { 0x95 }
						.lt { 0x9C }
						.gt { 0x9F }
						.le { 0x9E }
						.ge { 0x9D }
						else { 0x94 }
					}
					g.emit(0x0F)
					g.emit(u8(code))
					g.emit(0xC0) // setcc al
					g.emit(0x48)
					g.emit(0x0F)
					g.emit(0xB6)
					g.emit(0xC0) // movzx rax, al
				}
				else {}
			}
			g.store_reg_to_val(0, val_id)
		}
		.store {
			g.load_val_to_reg(0, instr.operands[0]) // Val -> RAX
			g.load_val_to_reg(1, instr.operands[1]) // Ptr -> RCX
			// mov [rcx], rax
			g.emit(0x48)
			g.emit(0x89)
			g.emit(0x01)
		}
		.load {
			g.load_val_to_reg(1, instr.operands[0]) // Ptr -> RCX
			// mov rax, [rcx]
			g.emit(0x48)
			g.emit(0x8B)
			g.emit(0x01)
			g.store_reg_to_val(0, val_id)
		}
		.alloca {
			off := g.alloca_offsets[val_id]
			// lea rax, [rbp + off]
			g.emit(0x48)
			g.emit(0x8D)
			if off >= -128 && off <= 127 {
				g.emit(0x45) // ModRM 01 = disp8
				g.emit(u8(off))
			} else {
				g.emit(0x85) // ModRM 10 = disp32
				g.emit_u32(u32(off))
			}
			g.store_reg_to_val(0, val_id)
		}
		.get_element_ptr {
			g.load_val_to_reg(0, instr.operands[0]) // Base -> RAX
			g.load_val_to_reg(1, instr.operands[1]) // Index -> RCX
			// Mimic arm64 logic: add rax, (rcx << 3)
			// shl rcx, 3
			g.emit(0x48)
			g.emit(0xC1)
			g.emit(0xE1)
			g.emit(0x03)
			// add rax, rcx
			g.emit(0x48)
			g.emit(0x01)
			g.emit(0xC8)
			g.store_reg_to_val(0, val_id)
		}
		.call {
			abi_regs := [7, 6, 2, 1, 8, 9]
			num_args := instr.operands.len - 1

			// Push stack arguments in reverse order (args 7+)
			mut stack_args := 0
			if num_args > 6 {
				stack_args = num_args - 6
				// Align stack to 16 bytes if odd number of stack args
				if stack_args % 2 == 1 {
					// push rax (shorter than sub rsp, 8)
					g.emit(0x50)
				}
				// Push in reverse order
				for i := num_args; i > 6; i-- {
					g.load_val_to_reg(0, instr.operands[i]) // RAX
					// push rax
					g.emit(0x50)
				}
			}

			// Load register arguments
			for i in 1 .. instr.operands.len {
				if i - 1 < 6 {
					g.load_val_to_reg(abi_regs[i - 1], instr.operands[i])
				}
			}
			fn_val := g.mod.values[instr.operands[0]]

			// xor eax, eax (Clear AL for variadic function calls)
			g.emit(0x31)
			g.emit(0xC0)

			g.emit(0xE8) // call rel32
			sym_idx := g.elf.add_undefined(fn_val.name)

			// Use R_X86_64_PLT32 (4) for function calls to support shared libraries (libc)
			g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 4, -4)
			g.emit_u32(0)

			// Clean up stack arguments
			if stack_args > 0 {
				cleanup := (stack_args + (stack_args % 2)) * 8
				// add rsp, cleanup
				g.emit(0x48)
				if cleanup <= 127 {
					g.emit(0x83)
					g.emit(0xC4)
					g.emit(u8(cleanup))
				} else {
					g.emit(0x81)
					g.emit(0xC4)
					g.emit_u32(u32(cleanup))
				}
			}

			if g.mod.type_store.types[g.mod.values[val_id].typ].kind != .void_t {
				g.store_reg_to_val(0, val_id)
			}
		}
		.ret {
			if instr.operands.len > 0 {
				g.load_val_to_reg(0, instr.operands[0])
			}
			// Cleanup Stack
			if g.stack_size > 0 {
				g.emit(0x48)
				if g.stack_size <= 127 {
					g.emit(0x83) // add r/m64, imm8
					g.emit(0xC4)
					g.emit(u8(g.stack_size))
				} else {
					g.emit(0x81) // add r/m64, imm32
					g.emit(0xC4)
					g.emit_u32(u32(g.stack_size))
				}
			}
			// Pop callee-saved regs (reverse order)
			for i := g.used_regs.len - 1; i >= 0; i-- {
				g.emit_pop_reg(g.used_regs[i])
			}
			// pop rbp; ret
			g.emit(0x5D)
			g.emit(0xC3)
		}
		.jmp {
			target_idx := g.mod.values[instr.operands[0]].index
			g.emit_jmp(target_idx)
		}
		.br {
			cond_id := instr.operands[0]
			true_blk := g.mod.values[instr.operands[1]].index
			false_blk := g.mod.values[instr.operands[2]].index

			// Test condition register directly if register-allocated
			if reg := g.reg_map[cond_id] {
				hw_reg := g.map_reg(reg)
				mut rex := u8(0x48)
				if hw_reg >= 8 {
					rex |= 5 // REX.RB
				}
				g.emit(rex)
				g.emit(0x85)
				g.emit(0xC0 | ((hw_reg & 7) << 3) | (hw_reg & 7))
			} else {
				g.load_val_to_reg(0, cond_id)
				g.emit(0x48)
				g.emit(0x85)
				g.emit(0xC0) // test rax, rax
			}

			// Emit je false_blk (jump if zero/false)
			g.emit(0x0F)
			g.emit(0x84) // je rel32
			g.record_pending_label(false_blk)
			g.emit_u32(0)
			// Jump to true block (can't assume it's the next block)
			g.emit_jmp(true_blk)
		}
		.switch_ {
			g.load_val_to_reg(0, instr.operands[0]) // RAX
			for i := 2; i < instr.operands.len; i += 2 {
				g.load_val_to_reg(1, instr.operands[i])
				g.emit(0x48)
				g.emit(0x39)
				g.emit(0xC8) // cmp rax, rcx
				g.emit(0x0F)
				g.emit(0x84) // je
				target_idx := g.mod.values[instr.operands[i + 1]].index
				if off := g.block_offsets[target_idx] {
					rel := off - (g.elf.text_data.len - g.curr_offset + 4)
					g.emit_u32(u32(rel))
				} else {
					g.record_pending_label(target_idx)
					g.emit_u32(0)
				}
			}
			def_idx := g.mod.values[instr.operands[1]].index
			g.emit_jmp(def_idx)
		}
		.assign {
			dest_id := instr.operands[0]
			src_id := instr.operands[1]
			g.load_val_to_reg(0, src_id)
			g.store_reg_to_val(0, dest_id)
		}
		.bitcast {
			if instr.operands.len > 0 {
				g.load_val_to_reg(0, instr.operands[0])
				g.store_reg_to_val(0, val_id)
			}
		}
		.phi {
			// Phi nodes are eliminated by optimization (converted to assignments)
			// but the instructions remain in the block. We ignore them here.
		}
		else {
			eprintln('x64: unknown op ${instr.op}')
		}
	}
}

fn (mut g Gen) emit_jmp(target_idx int) {
	g.emit(0xE9)
	if off := g.block_offsets[target_idx] {
		rel := off - (g.elf.text_data.len - g.curr_offset + 4)
		g.emit_u32(u32(rel))
	} else {
		g.record_pending_label(target_idx)
		g.emit_u32(0)
	}
}

fn (mut g Gen) load_val_to_reg(reg int, val_id int) {
	val := g.mod.values[val_id]
	if val.kind == .constant {
		if val.name.starts_with('"') {
			str_content := val.name.trim('"')
			// Handle escapes like arm64.v
			mut raw_bytes := []u8{}
			mut i := 0
			for i < str_content.len {
				if str_content[i] == `\\` && i + 1 < str_content.len {
					match str_content[i + 1] {
						`n` { raw_bytes << 10 }
						`t` { raw_bytes << 9 }
						`r` { raw_bytes << 13 }
						`\\` { raw_bytes << 92 }
						`"` { raw_bytes << 34 }
						`'` { raw_bytes << 39 }
						else { raw_bytes << str_content[i + 1] }
					}
					i += 2
				} else {
					raw_bytes << str_content[i]
					i++
				}
			}

			str_offset := g.elf.rodata.len
			g.elf.rodata << raw_bytes
			g.elf.rodata << 0
			sym_name := 'L_str_${g.curr_offset}_${str_offset}'
			sym_idx := g.elf.add_symbol(sym_name, u64(str_offset), false, 3)

			// lea reg, [rip + disp]
			hw_reg := g.map_reg(reg)
			mut rex := u8(0x48)
			if hw_reg >= 8 {
				rex |= 4
			}
			g.emit(rex)
			g.emit(0x8D)
			g.emit(0x05 | ((hw_reg & 7) << 3))
			g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
			g.emit_u32(0)
		} else {
			int_val := val.name.i64()
			hw_reg := g.map_reg(reg)
			if int_val == 0 {
				// xor reg, reg (2-3 bytes)
				if hw_reg >= 8 {
					g.emit(0x45) // REX.RB
					g.emit(0x31)
					g.emit(0xC0 | ((hw_reg & 7) << 3) | (hw_reg & 7))
				} else {
					g.emit(0x31)
					g.emit(0xC0 | (hw_reg << 3) | hw_reg)
				}
			} else if int_val > 0 && int_val <= 0x7FFFFFFF {
				// mov $imm32, %reg (5-6 bytes, zero-extends to 64-bit)
				if hw_reg >= 8 {
					g.emit(0x41) // REX.B
				}
				g.emit(0xB8 | (hw_reg & 7))
				g.emit_u32(u32(int_val))
			} else {
				// movabs $imm64, %reg (10 bytes)
				mut rex := u8(0x48)
				if hw_reg >= 8 {
					rex |= 1
				}
				g.emit(rex)
				g.emit(0xB8 | (hw_reg & 7))
				g.emit_u64(u64(int_val))
			}
		}
	} else if val.kind == .global {
		hw_reg := g.map_reg(reg)
		mut rex := u8(0x48)
		if hw_reg >= 8 {
			rex |= 4
		}
		g.emit(rex)
		g.emit(0x8D)
		g.emit(0x05 | ((hw_reg & 7) << 3))
		sym_idx := g.elf.add_undefined(val.name)
		g.elf.add_text_reloc(u64(g.elf.text_data.len), sym_idx, 2, -4)
		g.emit_u32(0)
	} else {
		if reg_idx := g.reg_map[val_id] {
			if reg_idx != reg {
				g.emit_mov_reg_reg(reg, reg_idx)
			}
		} else {
			offset := g.stack_map[val_id]
			g.emit_load_reg_mem(reg, offset)
		}
	}
}

fn (mut g Gen) store_reg_to_val(reg int, val_id int) {
	if reg_idx := g.reg_map[val_id] {
		if reg_idx != reg {
			g.emit_mov_reg_reg(reg_idx, reg)
		}
	} else {
		offset := g.stack_map[val_id]
		g.emit_store_reg_mem(reg, offset)
	}
}

fn (mut g Gen) emit_push_reg(reg int) {
	hw_reg := g.map_reg(reg)
	if hw_reg >= 8 {
		g.emit(0x41)
		g.emit(0x50 | (hw_reg & 7))
	} else {
		g.emit(0x50 | hw_reg)
	}
}

fn (mut g Gen) emit_pop_reg(reg int) {
	hw_reg := g.map_reg(reg)
	if hw_reg >= 8 {
		g.emit(0x41)
		g.emit(0x58 | (hw_reg & 7))
	} else {
		g.emit(0x58 | hw_reg)
	}
}

fn (mut g Gen) emit_mov_reg_reg(dst int, src int) {
	dst_hw := g.map_reg(dst)
	src_hw := g.map_reg(src)
	mut rex := u8(0x48)
	if src_hw >= 8 {
		rex |= 4
	}
	if dst_hw >= 8 {
		rex |= 1
	}
	g.emit(rex)
	g.emit(0x89)
	g.emit(0xC0 | ((src_hw & 7) << 3) | (dst_hw & 7))
}

fn (mut g Gen) emit_load_reg_mem(reg int, disp int) {
	hw_reg := g.map_reg(reg)
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 4
	}
	g.emit(rex)
	g.emit(0x8B)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3)) // ModRM 01 = disp8
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3)) // ModRM 10 = disp32
		g.emit_u32(u32(disp))
	}
}

fn (mut g Gen) emit_store_reg_mem(reg int, disp int) {
	hw_reg := g.map_reg(reg)
	mut rex := u8(0x48)
	if hw_reg >= 8 {
		rex |= 4
	}
	g.emit(rex)
	g.emit(0x89)
	if disp >= -128 && disp <= 127 {
		g.emit(0x45 | ((hw_reg & 7) << 3)) // ModRM 01 = disp8
		g.emit(u8(disp))
	} else {
		g.emit(0x85 | ((hw_reg & 7) << 3)) // ModRM 10 = disp32
		g.emit_u32(u32(disp))
	}
}

fn (g Gen) map_reg(r int) u8 {
	return u8(r)
}

fn (mut g Gen) emit(b u8) {
	g.elf.text_data << b
}

fn (mut g Gen) emit_u32(v u32) {
	g.emit(u8(v))
	g.emit(u8(v >> 8))
	g.emit(u8(v >> 16))
	g.emit(u8(v >> 24))
}

fn (mut g Gen) emit_u64(v u64) {
	g.emit_u32(u32(v))
	g.emit_u32(u32(v >> 32))
}

fn (mut g Gen) record_pending_label(blk int) {
	off := g.elf.text_data.len - g.curr_offset
	g.pending_labels[blk] << off
}

fn (mut g Gen) write_u32(off int, v u32) {
	binary.little_endian_put_u32(mut g.elf.text_data[off..off + 4], v)
}

pub fn (mut g Gen) write_file(path string) {
	g.elf.write(path)
}

// Register Allocation Logic

fn (mut g Gen) allocate_registers(func ssa.Function) {
	mut intervals := map[int]&Interval{}
	mut instr_idx := 0

	for pid in func.params {
		intervals[pid] = &Interval{
			val_id: pid
			start:  0
			end:    0
		}
	}

	// Track which values are alloca results - don't register allocate these
	// as they hold addresses that may be needed across the function
	mut alloca_vals := map[int]bool{}

	for blk_id in func.blocks {
		blk := g.mod.blocks[blk_id]
		for val_id in blk.instrs {
			val := g.mod.values[val_id]
			if val.kind == .instruction || val.kind == .argument {
				if unsafe { intervals[val_id] == nil } {
					intervals[val_id] = &Interval{
						val_id: val_id
						start:  instr_idx
						end:    instr_idx
					}
				}
			}
			instr := g.mod.instrs[val.index]
			// Mark alloca results as non-register-allocatable
			if instr.op == .alloca {
				alloca_vals[val_id] = true
			}
			for op in instr.operands {
				if g.mod.values[op].kind in [.instruction, .argument] {
					if mut interval := intervals[op] {
						if instr_idx > interval.end {
							interval.end = instr_idx
						}
					}
				}
			}
			instr_idx++
		}
	}

	mut sorted := []&Interval{}
	for _, i in intervals {
		sorted << i
	}
	sorted.sort(a.start < b.start)

	mut active := []&Interval{}
	// Use callee-saved registers: RBX(3), R12(12), R13(13), R14(14), R15(15)
	regs := [3, 12, 13, 14, 15]

	for i in sorted {
		// Skip alloca results - they must stay on stack to preserve addresses
		if alloca_vals[i.val_id] {
			continue
		}
		for j := 0; j < active.len; j++ {
			if active[j].end < i.start {
				active.delete(j)
				j--
			}
		}
		if active.len < regs.len {
			mut used := []bool{len: 16, init: false}
			for a in active {
				used[g.reg_map[a.val_id]] = true
			}
			for r in regs {
				if !used[r] {
					g.reg_map[i.val_id] = r
					active << i
					if r !in g.used_regs {
						g.used_regs << r
					}
					break
				}
			}
		}
	}
	g.used_regs.sort()
}
