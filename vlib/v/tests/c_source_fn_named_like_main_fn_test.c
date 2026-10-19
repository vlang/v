// These definitions share their names with V functions of the test's main module.
static int draw_glyph_run(int a,
	int b, int c) {
	return a * 100 + b * 10 + c;
}

int scaled_glyph_count(int a, int b) { return a * b; }

int c_source_glyph_total(int a) {
	return draw_glyph_run(a, 2, 3) + scaled_glyph_count(a, 7);
}
