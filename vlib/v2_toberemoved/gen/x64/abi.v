// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

pub enum X64Abi {
	sysv
	windows
}

const x64_no_arg_reg = -1

fn (abi X64Abi) int_arg_regs() []int {
	return match abi {
		.sysv { [int(rdi), int(rsi), int(rdx), int(rcx), int(r8), int(r9)] }
		.windows { [int(rcx), int(rdx), int(r8), int(r9)] }
	}
}

fn (abi X64Abi) sret_arg_regs() []int {
	return match abi {
		.sysv { [int(rsi), int(rdx), int(rcx), int(r8), int(r9)] }
		.windows { [int(rdx), int(r8), int(r9)] }
	}
}

fn (abi X64Abi) sret_reg() Reg {
	return match abi {
		.sysv { rdi }
		.windows { rcx }
	}
}

fn (abi X64Abi) float_arg_regs() []int {
	return match abi {
		.sysv { [0, 1, 2, 3, 4, 5, 6, 7] }
		.windows { [0, 1, 2, 3] }
	}
}

fn (abi X64Abi) uses_positional_arg_regs() bool {
	return abi == .windows
}

fn (abi X64Abi) int_arg_reg_at(index int) int {
	regs := abi.int_arg_regs()
	if index < 0 || index >= regs.len {
		return x64_no_arg_reg
	}
	return regs[index]
}

fn (abi X64Abi) float_arg_reg_at(index int) int {
	regs := abi.float_arg_regs()
	if index < 0 || index >= regs.len {
		return x64_no_arg_reg
	}
	return regs[index]
}

fn (abi X64Abi) int_arg_reg_for_position(arg_idx int) int {
	return match abi {
		.sysv { abi.int_arg_reg_at(arg_idx) }
		.windows { abi.int_arg_reg_at(arg_idx) }
	}
}

fn (abi X64Abi) float_arg_reg_for_position(arg_idx int) int {
	return match abi {
		.sysv { abi.float_arg_reg_at(arg_idx) }
		.windows { abi.float_arg_reg_at(arg_idx) }
	}
}

fn (abi X64Abi) shadow_space_size() int {
	return match abi {
		.sysv { 0 }
		.windows { 32 }
	}
}

fn (abi X64Abi) stack_arg_offset(position int) int {
	return match abi {
		.sysv { 16 + position * 8 }
		.windows { 48 + (position - 4) * 8 }
	}
}

fn (abi X64Abi) call_stack_arg_offset(position int) int {
	return match abi {
		.sysv { position * 8 }
		.windows { abi.shadow_space_size() + (position - 4) * 8 }
	}
}

fn (abi X64Abi) call_frame_size(stack_slots int) int {
	base := abi.shadow_space_size() + stack_slots * 8
	if base % 16 == 0 {
		return base
	}
	return base + (16 - (base % 16))
}
