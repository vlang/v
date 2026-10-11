// The element of `map`, `filter`, `any`, `all` and `count` was bound with the lowered
// element type of the array: `it` in `ids.map(it.str())` over a `[]UUID` was a
// `[16]u8`, so `it.str()` called the generated `str` of `[16]u8` instead of the one
// `UUID` declares. Other methods of the alias were found, because only `str` is
// lowered through the receiver's type.
import os

const vexe = @VEXE

const it_program = 'type UUID = [16]u8

type Count = int

fn (u UUID) str() string {
	return "uuid"
}

fn (c Count) str() string {
	return "count"
}

struct Holder {
	ids []UUID
}

fn main() {
	ids := [UUID{}, UUID{}]
	println(ids.map(it.str()))
	println(ids.map(|x| x.str()))
	println(ids.filter(it.str() == "uuid").len)
	println(ids.any(it.str() == "uuid"))
	println(ids.all(it.str() == "uuid"))
	println(ids.count(it.str() == "uuid"))
	counts := [Count(1), Count(2)]
	println(counts.map(it.str()))
	println(counts.filter(it.str() == "count").len)
	h := Holder{
		ids: ids
	}
	println(h.ids.map(it.str()))
	fixed := [UUID{}]!
	println(fixed.map(it.str()))
}
'

fn test_array_method_it_keeps_the_alias_of_the_elements() {
	dir := os.join_path(os.vtmp_dir(), 'v3_alias_str_array_it_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'main.v')
	os.write_file(src, it_program) or { panic(err) }
	exe := os.join_path(dir, 'main.exe')
	build := os.exec([vexe, '-new-compiler', '-o', exe, src])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ["['uuid', 'uuid']", "['uuid', 'uuid']", '2', 'true',
		'true', '2', "['count', 'count']", '2', "['uuid', 'uuid']", "['uuid']"]
}
