module fastc

// A large initializer body splits at statement lines, in order, without losing a byte.
fn test_fastc_split_startup_initializers_keeps_order() {
	mut body := ''
	for i in 0 .. 40 {
		body += '\tmain__table_${i} = (int[]){${i}, ${i + 1}, "a;{"};\n'
		body += '\tmain__init_${i}();\n'
	}
	parts := fastc_split_startup_initializers(body, 256)
	assert parts.len > 1
	assert parts.join('') == body
	for part in parts {
		assert part.ends_with('\n')
	}
}

// A small body stays one function.
fn test_fastc_split_startup_initializers_small_body() {
	body := '\tmain__x = 1;\n'
	assert fastc_split_startup_initializers(body, 256) == [body]
}

// A local declaration, a preprocessor line or a multi-line statement keeps the body whole: a
// later line may use the local, and a split would cut the statement or the condition.
fn test_fastc_split_startup_initializers_keeps_unmovable_body_whole() {
	filler := '\tmain__y = 2;\n'.repeat(40)
	for line in ['\tint tmp = 3;\n', '\tchar *s = 0;\n', '#if defined(X)\n', '\tmain__z = f(\n'] {
		body := filler + line + filler
		assert fastc_split_startup_initializers(body, 128) == [body]
	}
}

fn test_fastc_startup_line_is_movable() {
	assert fastc_startup_line_is_movable('\tmain__a = (Array){.len = 2};')
	assert fastc_startup_line_is_movable('\tmain__b = "})";')
	assert fastc_startup_line_is_movable("\tmain__c = '{';")
	assert fastc_startup_line_is_movable('\t*main__p = 1;')
	assert fastc_startup_line_is_movable('\tmain__init();')
	assert !fastc_startup_line_is_movable('\tint main__tmp = 1;')
	assert !fastc_startup_line_is_movable('\tArray *t;')
	assert !fastc_startup_line_is_movable('\tmain__d = {1,')
	assert !fastc_startup_line_is_movable('// comment')
}

// The zygote planner weighs the startup initializer parts, so that a unit holding them gets
// fewer bytes of them than of ordinary definitions.
fn test_fastc_zygote_solo_pieces_weigh_startup_parts() {
	part := 'x'.repeat(fastc_zygote_solo_item_bytes)
	pieces := ['head', part, part, part]
	units := FastcUnitLayout{
		head_end:            1
		solo_end:            4
		solo_starts:         [1, 2, 3]
		startup_parts_start: 2
		startup_parts_end:   3
	}
	mut sizes := []int{}
	solo_pieces := fastc_zygote_solo_pieces(pieces, &units, mut sizes)
	assert solo_pieces == [1, 2, 3, 4]
	assert sizes == [part.len, part.len * fastc_startup_initializer_cost, part.len]
}
