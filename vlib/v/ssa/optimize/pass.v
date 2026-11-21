module optimize

import v.ssa

// Pass is a single optimizer step. Implementations are stateless; shared
// pipeline state (target options, CFG snapshot, dominator info) travels in
// the PassContext the manager threads through the run.
pub interface Pass {
	// label names the step for verifier checkpoints. An empty label
	// disables the structured checkpoint after this pass.
	label() string
	// run executes the step against the module.
	run(mut m ssa.Module, mut ctx PassContext)
}

// PassContext carries the shared state passes need beyond the module:
// the pipeline options plus the CFG snapshot and dominator info that
// SSA construction computes once and reuses.
pub struct PassContext {
mut:
	opts OptimizeOptions
	cfg  CfgData
	dom  DomInfo
}

// PassManager runs an ordered list of passes with optional structured
// verification after every labeled pass.
pub struct PassManager {
pub mut:
	passes []Pass
}

// new creates a PassManager value for optimize.
pub fn PassManager.new(passes []Pass) PassManager {
	return PassManager{
		passes: passes
	}
}

// add registers one pass at the end of the pipeline.
pub fn (mut pm PassManager) add(p Pass) {
	pm.passes << p
}

// run executes every pass in order. When verification is requested it runs
// the structured verifier after each labeled pass, exactly where the
// previous handwritten pipeline put its checkpoints.
pub fn (mut pm PassManager) run(mut m ssa.Module, opts OptimizeOptions) {
	mut ctx := PassContext{
		opts: opts
	}
	for i in 0 .. pm.passes.len {
		mut p := pm.passes[i]
		p.run(mut m, mut ctx)
		if p.label() != '' && (opts.verify_each_pass || opts.strict_verify) {
			verify_and_panic_with_options(m, p.label(), VerifyPanicOptions{
				allow_noncritical: !opts.strict_verify
			})
		}
	}
}

// default_pipeline builds the production pass list for the given options.
// It performs the same steps, in the same order and under the same
// conditions, as the previous handwritten pipeline.
pub fn default_pipeline(opts OptimizeOptions) []Pass {
	mut passes := []Pass{}
	passes << NormalizePass{}
	if opts.mem2reg {
		passes << InlinePass{}
	}
	passes << FoldPass{}
	passes << BranchPass{}
	if opts.mem2reg {
		passes << PreSsaPass{}
		passes << DominatorsPass{}
		passes << Mem2regPass{}
		passes << SimplifyPhiPass{}
		passes << CsePass{}
		passes << MemoryPass{}
		passes << SimplifyPhi2Pass{}
	}
	if opts.eliminate_phis {
		passes << EliminatePhiPass{}
	}
	passes << CleanupPass{}
	passes << FinalizePass{}
	return passes
}

// NormalizePass rebuilds use-lists and the CFG, then runs the lenient
// verifier over the pipeline input.
struct NormalizePass {}

fn (p NormalizePass) label() string {
	return 'input'
}

fn (p NormalizePass) run(mut m ssa.Module, mut _ctx PassContext) {
	rebuild_use_lists(mut m)
	build_cfg(mut m)
	verify_ssa(m, 'initial normalization')
}

// InlinePass inlines small callees, then rebuilds use-lists and the CFG.
struct InlinePass {}

fn (p InlinePass) label() string {
	return 'inline'
}

fn (p InlinePass) run(mut m ssa.Module, mut _ctx PassContext) {
	inline_functions(mut m)
	rebuild_use_lists(mut m)
	build_cfg(mut m)
}

// FoldPass folds constants. No checkpoint follows it.
struct FoldPass {}

fn (p FoldPass) label() string {
	return ''
}

fn (p FoldPass) run(mut m ssa.Module, mut _ctx PassContext) {
	constant_fold(mut m)
}

// BranchPass folds branches, rebuilds the CFG, and reprunes phi operands.
// No checkpoint follows it.
struct BranchPass {}

fn (p BranchPass) label() string {
	return ''
}

fn (p BranchPass) run(mut m ssa.Module, mut _ctx PassContext) {
	branch_fold(mut m)
	build_cfg(mut m)
	prune_phi_operands(mut m)
}

// PreSsaPass normalizes the CFG before SSA construction: dead code and
// unreachable blocks go, remaining blocks merge, phis forget dead edges.
// No checkpoint follows it.
struct PreSsaPass {}

fn (p PreSsaPass) label() string {
	return ''
}

fn (p PreSsaPass) run(mut m ssa.Module, mut _ctx PassContext) {
	dead_code_elimination(mut m)
	remove_unreachable_blocks(mut m)
	merge_blocks(mut m)
	prune_phi_operands(mut m)
}

// DominatorsPass snapshots the CFG and computes dominator info for the
// passes that follow.
struct DominatorsPass {}

fn (p DominatorsPass) label() string {
	return 'compute_dominators'
}

fn (p DominatorsPass) run(mut m ssa.Module, mut ctx PassContext) {
	ctx.cfg = cfg_data_from_module(m)
	ctx.dom = compute_dominators(mut m, &ctx.cfg)
}

// Mem2regPass promotes scalar allocas to SSA values and phi nodes.
struct Mem2regPass {}

fn (p Mem2regPass) label() string {
	return 'mem2reg'
}

fn (p Mem2regPass) run(mut m ssa.Module, mut ctx PassContext) {
	promote_memory_to_register(mut m, ctx.dom, &ctx.cfg)
}

// SimplifyPhiPass simplifies phi nodes after SSA construction.
struct SimplifyPhiPass {}

fn (p SimplifyPhiPass) label() string {
	return 'simplify_phi'
}

fn (p SimplifyPhiPass) run(mut m ssa.Module, mut _ctx PassContext) {
	simplify_phi_nodes(mut m)
}

// CsePass folds again now that locals are in SSA form, then eliminates
// common subexpressions.
struct CsePass {}

fn (p CsePass) label() string {
	return 'cse'
}

fn (p CsePass) run(mut m ssa.Module, mut _ctx PassContext) {
	constant_fold(mut m)
	common_subexpression_elimination(mut m)
}

// MemoryPass optimizes memory operations on SSA-form locals.
struct MemoryPass {}

fn (p MemoryPass) label() string {
	return 'memory'
}

fn (p MemoryPass) run(mut m ssa.Module, mut _ctx PassContext) {
	optimize_memory(mut m)
}

// SimplifyPhi2Pass simplifies phi nodes once more after memory
// optimization. No checkpoint follows it.
struct SimplifyPhi2Pass {}

fn (p SimplifyPhi2Pass) label() string {
	return ''
}

fn (p SimplifyPhi2Pass) run(mut m ssa.Module, mut _ctx PassContext) {
	simplify_phi_nodes(mut m)
}

// EliminatePhiPass lowers phi nodes to assign copies for backends that
// cannot resolve phis natively.
struct EliminatePhiPass {}

fn (p EliminatePhiPass) label() string {
	return 'eliminate_phi'
}

fn (p EliminatePhiPass) run(mut m ssa.Module, mut _ctx PassContext) {
	eliminate_phi_nodes(mut m)
}

// CleanupPass removes dead code and unreachable blocks, merges or rebuilds
// the CFG depending on the mode, and reprunes phi operands.
// No checkpoint follows it.
struct CleanupPass {}

fn (p CleanupPass) label() string {
	return ''
}

fn (p CleanupPass) run(mut m ssa.Module, mut ctx PassContext) {
	dead_code_elimination(mut m)
	remove_unreachable_blocks(mut m)
	if ctx.opts.mem2reg {
		build_cfg(mut m)
	} else {
		merge_blocks(mut m)
	}
	prune_phi_operands(mut m)
}

// FinalizePass runs the lenient verifier over the pipeline output.
struct FinalizePass {}

fn (p FinalizePass) label() string {
	return 'final'
}

fn (p FinalizePass) run(mut m ssa.Module, mut _ctx PassContext) {
	verify_ssa(m, 'optimization')
}
