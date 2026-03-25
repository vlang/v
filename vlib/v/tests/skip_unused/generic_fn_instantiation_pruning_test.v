import os

const vexe = @VEXE

fn test_skip_unused_prunes_unused_generic_fn_instantiations() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_26019')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'issue_26019.v')
	source := [
		'module main',
		'',
		'fn new[T]() map[u8]T {',
		'\tx := map[u8]T{}',
		'\treturn x',
		'}',
		'',
		'fn not_used_fn() map[u8]int {',
		'\tx := new[int]()',
		'\treturn x',
		'}',
		'',
		'fn main() {',
		'\tx := new[u32]()',
		'\tdump(x)',
		'}',
	].join('\n')
	os.write_file(source_path, source) or { panic(err) }
	res := os.execute('${os.quoted_path(vexe)} -o - ${os.quoted_path(source_path)}')
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('VV_LOC Map_u8_u32 main__new_T_u32(void)')
	assert !res.output.contains('VV_LOC Map_u8_int main__new_T_int(void)')
}
