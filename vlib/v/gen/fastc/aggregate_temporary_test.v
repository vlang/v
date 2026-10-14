module fastc

import os
import v.cmdexec

fn test_fastc_aggregate_temporaries_have_addressable_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_aggregate_temporary_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Snapshot {
mut:
	amount int
}

struct Holder {
mut:
	value Snapshot
}

type HolderRef = &Holder

fn holder(value &Holder) &Holder {
	return value
}

fn alias_holder(value &Holder) HolderRef {
	return HolderRef(value)
}

fn snapshot() Snapshot {
	return Snapshot{amount: 42}
}

fn read(value &Snapshot) int {
	return value.amount
}

fn (value &Snapshot) get() int {
	return value.amount
}

fn (mut value Snapshot) bump() {
	value.amount++
}

fn pair(value int) !(int, int) {
	return value, value + 1
}

fn numbers() []int {
	return [3, 5, 8]
}

fn index() int {
	return 0
}

fn main() {
	first, second := pair(41) or { panic(err) }
	assert first == 41
	assert second == 42
	assert read(snapshot()) == 42
	assert snapshot().get() == 42
	mut local := Snapshot{amount: 4}
	(local).bump()
	assert local.amount == 5
	mut items := [Snapshot{amount: 10}]
	items[index()].bump()
	assert items[0].amount == 11
	mut original := Holder{value: Snapshot{amount: 20}}
	holder(&original).value.bump()
	assert original.value.amount == 21
	alias_holder(&original).value.bump()
	assert original.value.amount == 22
	values := numbers()[1..].clone()
	assert values == [5, 8]
	mut tables := map[string][]int{}
	tables["answer"] = [13, 21]
	copied := tables["answer"].clone()
	assert copied == [13, 21]
	println("temporaries work")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'temporaries work', run.output
}
