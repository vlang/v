import os

fn test_default_clone_treats_imported_enum_as_scalar() {
	root := os.join_path(os.temp_dir(), 'v3 default clone enum ${os.getpid()}')
	enum_dir := os.join_path(root, 'enum_mod')
	event_dir := os.join_path(root, 'event_mod')
	main_path := os.join_path(root, 'main.v')
	exe_suffix := $if windows { '.exe' } $else { '' }
	output_path := os.join_path(root, 'program${exe_suffix}')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(enum_dir) or { panic(err) }
	os.mkdir_all(event_dir) or { panic(err) }
	os.write_file(os.join_path(enum_dir, 'enum_mod.v'), 'module enum_mod
pub enum Result {
	ok = 3
	failed = 7
}
') or { panic(err) }
	os.write_file(os.join_path(event_dir, 'event_mod.v'), 'module event_mod
import enum_mod
pub struct Event {
pub:
	result enum_mod.Result
	name string
pub mut:
	values []int
}
pub fn count() int {
	mut events := []Event{}
	mut event := Event{
		result: .ok
		name: "ready".repeat(2)
		values: [1, 2, 3]
	}
	events << event
	assert events[0].result == .ok
	assert events[0].name == "readyready"
	assert events[0].name.str != event.name.str
	event.values[0] = 9
	assert events[0].values == [1, 2, 3]
	events[0].values[1] = 8
	assert event.values == [9, 2, 3]
	assert events[0].values == [1, 8, 3]
	assert event.result == .ok
	return events.len
}
') or { panic(err) }
	os.write_file(main_path, 'module main
import event_mod
import os
import enum_mod

type Result = enum_mod.Result
type ResultAlias = Result

struct Event {
	result ResultAlias
	name string
mut:
	values []int
}

fn main() {
	_ = os.Result{}
	assert event_mod.count() == 1
	mut event := Event{
		result: ResultAlias(Result(enum_mod.Result.failed))
		name: "ready".repeat(2)
		values: [1, 2, 3]
	}
	mut events := []Event{}
	events << event
	assert events[0].result == ResultAlias(Result(enum_mod.Result.failed))
	assert events[0].name == "readyready"
	assert events[0].name.str != event.name.str
	event.values[0] = 9
	assert events[0].values == [1, 2, 3]
	events[0].values[1] = 8
	assert event.values == [9, 2, 3]
	assert events[0].values == [1, 8, 3]
	assert event.result == ResultAlias(Result(enum_mod.Result.failed))
}
') or { panic(err) }
	compile := os.exec([@VEXE, '-new-compiler', '-path', '${root}' + '|@vlib|@vmodules', '-o',
		output_path, main_path])
	assert compile.exit_code == 0, compile.output
	run := os.exec([output_path])
	assert run.exit_code == 0, run.output
}
