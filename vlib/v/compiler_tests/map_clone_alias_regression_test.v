import os

fn test_map_clone_entry_exception_preserves_alias_diagnostics() {
	root := os.join_path(os.temp_dir(), 'v_map_clone_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v_dir := os.dir(os.dir(@FILE))
	vlib_dir := os.dir(v_dir)
	compiler := os.join_path(root, 'v3')
	compiler_source := os.join_path(v_dir, 'v.v')
	vexe := @VEXE
	build := os.execute('"${vexe}" -gc none -path "${vlib_dir}|@vlib|@vmodules" -o "${compiler}" "${compiler_source}"')
	assert build.exit_code == 0, build.output

	header := 'struct Row {
mut:
	cells []int
}

fn borrowed(values map[string]Row) map[string]Row {
	return values
}
'
	cases := {
		'nested': header + '
fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	copy["new"] = Row{cells: [2]}
	copy["old"].cells[0] = 9
}
'
		'rebound': header + '
fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	copy = borrowed(scope)
	copy["new"] = Row{cells: [2]}
}
'
		'loop_rebound': header + '
fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	for i in 0 .. 2 {
		copy["new"] = Row{cells: [2]}
		if i == 0 {
			copy = borrowed(scope)
		}
	}
}
'
		'mutable_escape': header + '
fn replace_map(mut dst map[string]Row, src map[string]Row) {
	dst = borrowed(src)
}

fn main() {
	scope := {"old": Row{cells: [1]}}
	mut copy := scope.clone()
	replace_map(mut copy, scope)
	copy["new"] = Row{cells: [2]}
}
'
		'user_clone': header + '
struct Holder {
	values map[string]Row
}

fn (holder Holder) clone() map[string]Row {
	return holder.values
}

fn main() {
	holder := Holder{values: {"old": Row{cells: [1]}}}
	mut copy := holder.clone()
	copy["new"] = Row{cells: [2]}
}
'
	}
	for name, source in cases {
		path := os.join_path(root, '${name}.v')
		os.write_file(path, source) or { panic(err) }
		output := os.join_path(root, name)
		result := os.execute('"${compiler}" -nocache -b c -o "${output}" "${path}"')
		assert result.exit_code != 0, '${name} unexpectedly compiled'
		assert result.output.contains('aliases mutable data from an immutable value'), '${name}: ${result.output}'
		if name == 'nested' {
			assert result.output.contains('`copy["old"].cells` aliases mutable data'), result.output
		}
	}
}
