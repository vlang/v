// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module insel

import v2.mir
import v2.pref
import v2.ssa

// select performs target-specific instruction selection on MIR.
// The selected opcode is currently encoded as a stable textual tag so later
// layers can consume it without changing existing backend contracts.
pub fn select(mut m mir.Module, arch pref.Arch) {
	prefix := target_prefix(arch)
	for i := 0; i < m.instrs.len; i++ {
		mut ins := &m.instrs[i]
		ins.selected_op = select_op_name(prefix, ins.op)
	}
}

fn target_prefix(arch pref.Arch) string {
	return match arch {
		.arm64 { 'arm64' }
		.x64 { 'x64' }
		else { 'native' }
	}
}

fn select_op_name(prefix string, op ssa.OpCode) string {
	suffix := match op {
		.add { 'add_rr' }
		.sub { 'sub_rr' }
		.mul { 'mul_rr' }
		.sdiv { 'sdiv_rr' }
		.and_ { 'and_rr' }
		.or_ { 'or_rr' }
		.xor { 'xor_rr' }
		.load { 'load_mr' }
		.store { 'store_rm' }
		.call { 'call' }
		.call_indirect { 'call_indirect' }
		.call_sret { 'call_sret' }
		.ret { 'ret' }
		.br { 'br' }
		.jmp { 'jmp' }
		.switch_ { 'switch' }
		.assign { 'copy' }
		else { op.str() }
	}
	return '${prefix}.${suffix}'
}
