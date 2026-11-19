module optimize

import os
import v.ssa

// OptimizeOptions controls optimize options behavior used by optimize.
pub struct OptimizeOptions {
pub:
	mem2reg          bool // promote scalar allocas to SSA values + phi nodes
	eliminate_phis   bool // lower phis to `assign` copies (for backends without phi)
	verify_each_pass bool // run the structured verifier after every structural pass
	strict_verify    bool // treat historically-noncritical verifier findings as fatal
}

// optimize runs the production optimization pipeline. Scalar alloca promotion
// and phi elimination are kept together because the ARM64 backend consumes the
// resulting edge copies rather than native phi nodes.
//   V3_VERIFY=1    structured verify after each pass (V3_VERIFY_STRICT=1 = fatal)
pub fn optimize(mut m ssa.Module) {
	optimize_with_options(mut m, OptimizeOptions{
		mem2reg:          true
		eliminate_phis:   true
		verify_each_pass: os.getenv('V3_VERIFY') != ''
		strict_verify:    os.getenv('V3_VERIFY_STRICT') != ''
	})
}

// optimize_with_options supports optimize with options handling for optimize.
// It runs the default pass pipeline through a PassManager; the pipeline
// performs the same steps, in the same order and under the same
// conditions, as the previous handwritten sequence.
pub fn optimize_with_options(mut m ssa.Module, opts OptimizeOptions) {
	m.track_uses = true
	mut pm := PassManager.new(default_pipeline(opts))
	pm.run(mut m, opts)
}

// rebuild_use_lists supports rebuild use lists handling for optimize.
fn rebuild_use_lists(mut m ssa.Module) {
	for vi in 0 .. m.values.len {
		mut val := m.values[vi]
		val.uses = []
		m.values[vi] = val
	}
	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			for val_id in m.blocks[blk_id].instrs {
				if val_id <= 0 || val_id >= m.values.len || m.values[val_id].kind != .instruction {
					continue
				}
				instr_idx := m.values[val_id].index
				if instr_idx < 0 || instr_idx >= m.instrs.len {
					continue
				}
				instr := m.instrs[instr_idx]

				for oi, op_id in instr.operands {
					if !instr.is_value_operand(oi) {
						continue
					}
					mut already_recorded := false
					for previous in 0 .. oi {
						if instr.is_value_operand(previous) && instr.operands[previous] == op_id {
							already_recorded = true
							break
						}
					}
					if op_id >= 0 && op_id < m.values.len && !already_recorded
						&& m.values[op_id].has_use_list() {
						mut op_val := m.values[op_id]
						op_val.uses << val_id
						m.values[op_id] = op_val
					}
				}
			}
		}
	}
}
