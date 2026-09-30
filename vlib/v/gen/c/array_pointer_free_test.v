module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn test_c_free_preserves_the_allocator_for_aligned_array_pointer_casts() {
	path := os.join_path(os.vtmp_dir(), 'aligned_array_free_${os.getpid()}.c.v')
	os.write_file(path, 'fn C.malloc(size usize) voidptr
fn C.free(value voidptr)
@[aligned: 8]
struct AlignedCell { value int }
type Cells = [2]AlignedCell
fn release_c(value &Cells) { unsafe { C.free(value) } }
fn release_v(value &Cells) { unsafe { free(value) } }
fn main() {
	ordinary := unsafe { &Cells(C.malloc(sizeof(Cells))) }
	release_c(ordinary)
	owned := unsafe { &Cells(malloc(sizeof(Cells))) }
	release_v(owned)
	aligned := &Cells{}
	release_v(aligned)
}
')!
	defer { os.rm(path) or {} }
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used := markused.mark_used(a, tc)
	mut g := FlatGen.new()
	g.set_target(pref.target_from('windows', 'amd64') or { panic(err) })
	generated := g.gen_with_used_options(a, used, &tc, true)
	c_release := generated.all_after('void release_c(Array_fixed_main__AlignedCell_2* value) {').all_before('\n}').trim_space()
	assert c_release.contains('free(value);'), c_release
	assert !c_release.contains('v3_aligned_free'), c_release
	v_release := generated.all_after('void release_v(Array_fixed_main__AlignedCell_2* value) {').all_before('\n}').trim_space()
	assert v_release.contains('v3_aligned_free(value);'), v_release
	assert generated.contains('v_malloc(sizeof(main__AlignedCell[2]))'), generated
	assert generated.contains('v3_aligned_memdup('), generated
}
