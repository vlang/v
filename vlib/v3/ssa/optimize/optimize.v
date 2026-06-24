module optimize

import os
import v3.ssa

// OptimizeOptions controls optimize options behavior used by optimize.
pub struct OptimizeOptions {
pub:
	mem2reg          bool // promote scalar allocas to SSA values + phi nodes
	eliminate_phis   bool // lower phis to `assign` copies (for backends without phi)
	verify_each_pass bool // run the structured verifier after every structural pass
	strict_verify    bool // treat historically-noncritical verifier findings as fatal
}

// optimize runs the default, backend-safe optimization pipeline. Structural SSA
// construction (mem2reg / phi elimination) is opt-in via the environment so the
// proven arm64 lowering path is unchanged unless explicitly requested:
//   V3_MEM2REG=1   enable alloca promotion + phi insertion
//   V3_PHI_ELIM=1  additionally lower phis to assign copies
//   V3_VERIFY=1    structured verify after each pass (V3_VERIFY_STRICT=1 = fatal)
pub fn optimize(mut m ssa.Module) {
	mem2reg := os.getenv('V3_MEM2REG') != ''
	optimize_with_options(mut m, OptimizeOptions{
		mem2reg: mem2reg
		// The arm64 backend's native phi resolution is incomplete (no copies on
		// conditional edges, no parallel-copy sequencing), so phis introduced by
		// mem2reg must be lowered to assign copies for correct codegen.
		eliminate_phis:   mem2reg || os.getenv('V3_PHI_ELIM') != ''
		verify_each_pass: os.getenv('V3_VERIFY') != ''
		strict_verify:    os.getenv('V3_VERIFY_STRICT') != ''
	})
}

// optimize_with_options supports optimize with options handling for optimize.
pub fn optimize_with_options(mut m ssa.Module, opts OptimizeOptions) {
	rebuild_use_lists(mut m)
	build_cfg(mut m)
	verify_ssa(m, 'initial normalization')
	verify_pipeline_checkpoint(m, opts, 'input')

	constant_fold(mut m)
	rebuild_use_lists(mut m)

	branch_fold(mut m)
	rebuild_use_lists(mut m)
	build_cfg(mut m)
	// Branch folding can drop a phi block's predecessor edge; keep phis consistent.
	prune_phi_operands(mut m)
	rebuild_use_lists(mut m)

	if opts.mem2reg {
		// Normalize the CFG *before* SSA construction so that every phi
		// predecessor (a reachable CFG predecessor) stays present in the
		// function afterwards. Block-removing passes must not run once phis
		// exist, or their predecessor operands would dangle.
		dead_code_elimination(mut m)
		rebuild_use_lists(mut m)
		build_cfg(mut m)
		remove_unreachable_blocks(mut m)
		merge_blocks(mut m)
		rebuild_use_lists(mut m)
		build_cfg(mut m)

		// Structural SSA construction: dominators -> mem2reg -> simplify phis.
		cfg := cfg_data_from_module(m)
		dom := compute_dominators(mut m, &cfg)
		verify_pipeline_checkpoint(m, opts, 'compute_dominators')
		promote_memory_to_register(mut m, dom, &cfg)
		rebuild_use_lists(mut m)
		verify_pipeline_checkpoint(m, opts, 'mem2reg')
		simplify_phi_nodes(mut m)
		rebuild_use_lists(mut m)
		build_cfg(mut m)
		verify_pipeline_checkpoint(m, opts, 'simplify_phi')
	}

	// Phi elimination lowers phis to assign copies for backends that cannot
	// resolve phis natively. Runs whenever requested (input phis from the builder
	// or a worker merge may exist even without mem2reg).
	if opts.eliminate_phis {
		eliminate_phi_nodes(mut m)
		rebuild_use_lists(mut m)
		build_cfg(mut m)
		verify_pipeline_checkpoint(m, opts, 'eliminate_phi')
	}

	dead_code_elimination(mut m)
	rebuild_use_lists(mut m)
	build_cfg(mut m)

	remove_unreachable_blocks(mut m)
	if !opts.mem2reg {
		// Without SSA construction, block-merging is phi-aware and safe to run.
		merge_blocks(mut m)
	}
	rebuild_use_lists(mut m)
	build_cfg(mut m)

	// Final phi-consistency pass: any phi still present must match the final CFG.
	prune_phi_operands(mut m)
	rebuild_use_lists(mut m)
	build_cfg(mut m)

	verify_ssa(m, 'optimization')
	verify_pipeline_checkpoint(m, opts, 'final')
}

// verify_pipeline_checkpoint validates verify pipeline checkpoint state for optimize.
fn verify_pipeline_checkpoint(m &ssa.Module, opts OptimizeOptions, pass_name string) {
	if opts.verify_each_pass || opts.strict_verify {
		verify_and_panic_with_options(m, pass_name, VerifyPanicOptions{
			allow_noncritical: !opts.strict_verify
		})
	}
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

				for op_id in instr.value_operands() {
					if op_id >= 0 && op_id < m.values.len && val_id !in m.values[op_id].uses {
						mut op_val := m.values[op_id]
						op_val.uses << val_id
						m.values[op_id] = op_val
					}
				}
			}
		}
	}
}
