module main

import os

fn test_c_free_preserves_the_allocator_for_aligned_array_pointer_casts() {
	root := os.join_path(os.vtmp_dir(), 'aligned_array_free_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.c.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(path, 'fn C.malloc(size uint) voidptr
fn C.free(value voidptr)
@[aligned: 8]
struct AlignedCell { value int }
type Cells = [2]AlignedCell
fn release_c(value &Cells) { unsafe { C.free(value) } }
fn release_v(value &Cells) { unsafe { free(value) } }
fn main() {
	ordinary := unsafe { &Cells(C.malloc(sizeof(Cells))) }
	release_c(ordinary)
	owned := unsafe { &Cells(malloc(int(sizeof(Cells)))) }
	release_v(owned)
	aligned := &Cells{}
	release_v(aligned)
}
')!
	// V1-hosted unit tests get a shared V3 compiler from their runner.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE, '-new-compiler'] }
	args << ['-gc', 'none', '-os', 'windows', '-arch', 'amd64', '-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(c_path)!
	c_release := generated.all_after('void release_c(Array_fixed_main__AlignedCell_2* value) {').all_before('\n}').trim_space()
	assert c_release.contains('free(value);'), c_release
	assert !c_release.contains('v3_aligned_free'), c_release
	v_release := generated.all_after('void release_v(Array_fixed_main__AlignedCell_2* value) {').all_before('\n}').trim_space()
	assert v_release.contains('v3_aligned_free(value);'), v_release
	assert generated.contains('v_malloc((i64)(sizeof(main__AlignedCell[2])))'), generated
	assert generated.contains('aligned = (Array_fixed_main__AlignedCell_2*)v3_aligned_memdup('), generated
}
