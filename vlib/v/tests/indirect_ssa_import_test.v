// v.mir and v.insel expose v.ssa types, but this file does not import v.ssa.
// Ordinary programs are compiled with the `skip_ssa` define, which only drops the
// compiler's own gated SSA imports; v.ssa must still be compiled in for v.mir.
import v.insel
import v.mir

fn test_mir_and_insel_without_a_direct_ssa_import() {
	mut m := mir.Module{
		target: mir.arm64_target()
	}
	assert m.target.arch == .arm64
	assert m.target.pointer_size == 8
	assert m.funcs.len == 0
	sel := insel.select_(mut m)
	assert sel.target.arch == .arm64
	assert sel.funcs.len == 0
}
