import os
import v.tempname

fn test_map_receiver_method_is_rooted_by_markused() {
	dir := os.join_path(os.vtmp_dir(), 'v3_map_receiver_method_markused_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'v3_map_receiver_method_markused_input')
	src := bin + '.v'
	os.write_file(src, "struct Entry {
	kind int
}

fn (m map[string]Entry) find(name string) ?Entry {
	if name in m {
		return m[name]
	}
	return none
}

fn main() {
	mut items := map[string]Entry{}
	items['name'] = Entry{
		kind: 41
	}
	got := items.find('name') or { return }
	println(got.kind)
}
") or {
		panic(err)
	}

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('map_stringEntry__find'), generated

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '41'
}
