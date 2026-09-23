import os

fn test_default_clone_treats_imported_enum_as_scalar() {
	root := os.join_path(os.temp_dir(), 'v3_default_clone_enum_${os.getpid()}')
	enum_dir := os.join_path(root, 'enum_mod')
	event_dir := os.join_path(root, 'event_mod')
	main_path := os.join_path(root, 'main.v')
	output_path := os.join_path(root, 'program')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(enum_dir) or { panic(err) }
	os.mkdir_all(event_dir) or { panic(err) }
	os.write_file(os.join_path(enum_dir, 'enum_mod.v'), 'module enum_mod
pub enum Result { ok failed }
') or { panic(err) }
	os.write_file(os.join_path(event_dir, 'event_mod.v'), 'module event_mod
import enum_mod
pub struct Event {
pub:
	result enum_mod.Result
	name string
}
pub fn count() int {
	mut events := []Event{}
	event := Event{result: .ok, name: "ready"}
	events << event
	return events.len
}
') or { panic(err) }
	os.write_file(main_path, 'module main
import event_mod
import os
fn main() {
	_ = os.Result{}
	assert event_mod.count() == 1
}
') or { panic(err) }
	compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -path "${root}|@vlib|@vmodules" -o ${os.quoted_path(output_path)} ${os.quoted_path(main_path)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(output_path))
	assert run.exit_code == 0, run.output
}
