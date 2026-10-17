// An included C source defines `draw_glyph_run` and `scaled_glyph_count` with other
// signatures. The V functions below must keep their own C symbols (`main__name`, as
// in V 0.5.2), instead of conflicting with the C definitions in the same unit.
#include "@VEXEROOT/vlib/v/tests/c_source_fn_named_like_main_fn_test.c"

fn C.c_source_glyph_total(a i32) i32

fn draw_glyph_run(text string, scale f32) bool {
	return text.len > 0 && scale > 0
}

fn scaled_glyph_count(text string) int {
	return text.len * 2
}

fn call_glyph_run(run fn (string, f32) bool, text string) bool {
	return run(text, 2.0)
}

fn test_main_fns_named_like_c_source_fns() {
	assert draw_glyph_run('abc', 1.5)
	assert !draw_glyph_run('', 1.5)
	assert call_glyph_run(draw_glyph_run, 'x')
	assert scaled_glyph_count('abcd') == 8
}

fn test_c_source_fns_keep_their_definitions() {
	assert C.c_source_glyph_total(1) == 123 + 7
	assert C.c_source_glyph_total(2) == 223 + 14
}
