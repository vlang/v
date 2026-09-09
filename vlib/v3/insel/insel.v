module insel

import v3.mir
import v3.ssa

// OperandKind lists operand kind values used by insel.
pub enum OperandKind {
	value
	block
}

// Operand represents operand data used by insel.
pub struct Operand {
pub:
	kind OperandKind
	id   int
}

// Instruction represents instruction data used by insel.
pub struct Instruction {
pub:
	op       ssa.OpCode
	operands []Operand
	typ      ssa.TypeID
}

// BasicBlock represents basic block data used by insel.
pub struct BasicBlock {
pub:
	id     int
	name   string
	instrs []Instruction
}

// Function represents function data used by insel.
pub struct Function {
pub:
	id     int
	name   string
	abi    mir.FunctionAbi
	blocks []BasicBlock
}

// Module represents module data used by insel.
pub struct Module {
pub:
	target mir.Target
	funcs  []Function
}

// select_ selects MIR instructions into a target-specific machine module shell.
pub fn select_(mut m mir.Module) Module {
	mut funcs := []Function{}
	for f in m.funcs {
		if f.is_c_extern {
			continue
		}
		mut blocks := []BasicBlock{}
		for block_id in f.blocks {
			if block_id < 0 || block_id >= m.blocks.len {
				continue
			}
			blk := m.blocks[block_id]
			mut instrs := []Instruction{}
			for val_id in blk.instrs {
				if val_id <= 0 || val_id >= m.values.len {
					continue
				}
				val := m.values[val_id]
				if val.kind != .instruction {
					continue
				}
				instr := m.instrs[val.index]
				instrs << Instruction{
					op:       instr.op
					operands: select_operands(instr)
					typ:      instr.typ
				}
			}
			blocks << BasicBlock{
				id:     blk.id
				name:   blk.name
				instrs: instrs
			}
		}
		funcs << Function{
			id:     f.id
			name:   f.name
			abi:    f.abi
			blocks: blocks
		}
	}
	return Module{
		target: m.target
		funcs:  funcs
	}
}

// select_operands resolves select operands information for insel.
fn select_operands(instr mir.Instruction) []Operand {
	mut operands := []Operand{}
	for i, operand in instr.operands {
		kind := if instr.op == .br && i > 0 {
			OperandKind.block
		} else if instr.op == .phi && i % 2 == 1 {
			OperandKind.block
		} else if instr.op == .jmp {
			OperandKind.block
		} else {
			OperandKind.value
		}
		operands << Operand{
			kind: kind
			id:   operand
		}
	}
	return operands
}
