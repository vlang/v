// `sort` looked for the `<` of the elements only on structs. The elements of a
// `[]UUID` are lowered as `[16]u8`, so `ids.sort(a < b)` compared two fixed arrays
// instead of calling the `<` that the alias declares, and `scores.sort()` of an alias of
// `int` took the runtime int sort. The `<` of an alias was already called for a plain
// `a < b`.
import os

const vexe = @VEXE

const sort_program = 'type UUID = [2]u8

type Score = int

fn (a UUID) < (b UUID) bool {
	return a[0] < b[0] || (a[0] == b[0] && a[1] < b[1])
}

fn (a Score) < (b Score) bool {
	return int(a) > int(b)
}

fn main() {
	mut ids := [UUID([u8(9), 0]!), UUID([u8(1), 5]!), UUID([u8(1), 2]!)]
	ids.sort(a < b)
	println(ids.map(it[0] * 10 + it[1]))
	ids.sort(b < a)
	println(ids.map(it[0] * 10 + it[1]))
	ids.sort()
	println(ids.map(it[0] * 10 + it[1]))
	sorted := ids.sorted(b < a)
	println(sorted.map(it[0] * 10 + it[1]))
	mut scores := [Score(1), Score(3), Score(2)]
	scores.sort(a < b)
	println(scores.map(int(it)))
	scores = [Score(1), Score(3), Score(2)]
	scores.sort()
	println(scores.map(int(it)))
	println([Score(2), Score(1), Score(3)].sorted().map(int(it)))
}
'

fn test_sort_calls_the_less_than_of_an_alias() {
	dir := os.join_path(os.vtmp_dir(), 'v3_alias_sort_operator_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'main.v')
	os.write_file(src, sort_program) or { panic(err) }
	exe := os.join_path(dir, 'main.exe')
	build := os.exec([vexe, '-new-compiler', '-o', exe, src])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['[12, 15, 90]', '[90, 15, 12]', '[12, 15, 90]',
		'[90, 15, 12]', '[3, 2, 1]', '[3, 2, 1]', '[3, 2, 1]']
}
