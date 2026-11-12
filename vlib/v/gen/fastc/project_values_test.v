module fastc

import os
import v.cmdexec

fn test_fastc_project_values_keep_context_and_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_project_values_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'units')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'units', 'units.v'), r'module units
pub type Distance = f64
pub fn Distance.new(value int) Distance { return Distance(value) }
pub fn (value Distance) to_int() int { return int(value) }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import units

type Length = units.Distance
enum Direction { forward backward none }
enum Selection { forward backward none }
enum Axis { x y }
struct Snapshot { direction Selection }
struct Item { value int }
struct Group { mut: values []int flags u32 }
struct Record { mut: groups map[string]Group }
struct Matrix { values [2]int }
type Payload = Item | Group
struct Box { payload Payload }

fn snapshot(direction Direction) Snapshot {
 return Snapshot{direction: match direction {
  .forward { .forward }
  .backward { .backward }
  .none { .none }
 }}
}
fn selection(direction Selection) Selection { return direction }
fn convert(direction Direction) Selection {
 return selection(match direction {
  .forward { .forward }
  .backward { .backward }
  .none { .none }
 })
}
fn length(axis Axis, value Length) Length {
 return if axis == .x { value } else { Length.new(0) }
}
fn mutable_option(mut value ?&Item) int {
 if item := value { return item.value }
 return 0
}
fn ordered_key(mut order []int, value int) int { order << value; return value }
fn item_value(value Item) int { return value.value }
fn boxed_value(box &Box) int {
 match box.payload {
  Item { return item_value(box.payload) }
  Group { return box.payload.values.len }
 }
}
fn choose(value int) int {
 return match value {
  0 { 10 } 1 { 11 } 2 { 12 } 3 { 13 } 4 { 14 }
  5 { 15 } 6 { 16 } 7 { 17 } 8 { 18 } 9 { 19 }
  10 { 20 } 11 { 21 } 12 { 22 } 13 { 23 } 14 { 24 }
  15 { 25 } 16 { 26 } 17 { 27 } 18 { 28 } 19 { 29 }
  else { -1 }
 }
}
fn main() {
 assert snapshot(.none).direction == .none
 assert snapshot(.forward).direction == .forward
 assert convert(.backward) == .backward
 assert length(.x, Length.new(42)).to_int() == 42
 mut rows := [Record{groups: {"a": Group{flags: 1}}}]
 rows[0].groups["a"].values << 42
 assert rows[0].groups["a"].values == [42]
 assert rows[0].groups["a"].flags & 1 != 0
 assert rows[0].groups["missing"].flags == 0
 assert rows[0].groups.len == 1
 mut order := []int{}
 mut nested := {1: {2: Group{flags: 1}}}
 assert nested[ordered_key(mut order, 1)][ordered_key(mut order, 2)].flags == 1
 assert order == [1, 2]
 order.clear()
 nested[ordered_key(mut order, 1)][ordered_key(mut order, 2)].values << 7
 assert order == [1, 2]
 assert nested[1][2].values == [7]
 values := [1, 2, 3]
 assert values[1..] == [2, 3]
 matrix := Matrix{values: [1, 2]!}
 updated := Matrix{...matrix, values: [3, 4]!}
 assert updated.values[0] == 3
 assert matrix.values[0] == 1
 mut options := []?&Item{len: 2, init: none}
 assert options[0] == none
 options[1] = &Item{42}
 item := options[1] or { panic("missing item") }
 assert item.value == 42
 mut optional := ?&Item(&Item{42})
 assert mutable_option(mut optional) == 42
 assert boxed_value(&Box{payload: Item{42}}) == 42
 for i in 0 .. 20 { assert choose(i) == i + 10 }
 assert choose(20) == -1
}
') or { panic(err) }
	for compiler in ['', 'clang'] {
		if compiler == 'clang' && os.find_abs_path_of_executable('clang') == none { continue }
		program := os.join_path(root, 'program_' + compiler)
		mut args := ['-b', 'fastc', '-nocache', '-o', program]
		if compiler != '' { args << ['-cc', compiler] }
		args << root
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
