import os

const vroot = os.dir(@VEXE)
const test_vexe = os.quoted_path(@VEXE)

// A map-of-map index is the one lookup that has to build its default lazily, so
// that a hit never constructs - and never leaks - a map header. It used to do
// that with a `({ ... })` statement expression, which is a GNU extension: MSVC
// rejects it outright, and the whole `makev.bat -msvc` build failed on it.
fn nested_map_generated_c() string {
	src := os.join_path(os.vtmp_dir(), 'v_map_value_lookup_${os.getpid()}.v')
	os.write_file(src, "fn main() {
	mut m := map[string]map[string]int{}
	m['a']['x'] = 1
	println(m['a']['x'])
	println(m['b'].len)
}
") or { panic(err) }
	defer {
		os.rm(src) or {}
	}
	cmd := '${test_vexe} -old-compiler -o - ${os.quoted_path(src)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	return res.output
}

fn test_nested_map_lookup_does_not_emit_a_statement_expression() {
	generated := nested_map_generated_c()
	assert !generated.contains('({ map*'), generated
}

fn test_nested_map_lookup_calls_the_generated_helpers() {
	generated := nested_map_generated_c()
	// The reading lookup returns the value, the mutating one returns a pointer
	// into the map so the nested assignment can write through it.
	assert generated.contains('_v_map_get_or_zero_')
	assert generated.contains('_v_map_get_or_insert_')
	assert generated.contains('builtin__map_get_check(m, key)')
}
