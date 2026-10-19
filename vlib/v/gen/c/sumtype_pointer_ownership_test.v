module c

import os

fn test_sumtype_pointer_owners_move_once_and_borrows_remain_copyable() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'sumtype_pointer_ownership_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	compiler := os.join_path(dir, 'v-ownership')
	vlib := os.join_path(@VEXEROOT, 'vlib')
	build := os.exec([@VEXE, '-new-compiler', '-b', 'c', '-cc', cc, '-gc', 'none', '-d', 'ownership',
		'-path', vlib, '-o', compiler, os.join_path(vlib, 'v', 'v.v')])
	assert build.exit_code == 0, build.output
	header := os.join_path(dir, 'drops.h')
	os.write_file(header, '#include <assert.h>
static int drop_counts[16];
static void* dropped_addresses[16];
static void sum_record_drop(int code, void* address) {
 assert(code > 0 && code < 16);
 assert(drop_counts[code] == 0);
 drop_counts[code]++;
 dropped_addresses[code] = address;
}
static int sum_drop_count(int code) { return drop_counts[code]; }
static void* sum_last_drop(int code) { return dropped_addresses[code]; }
')!
	source := os.join_path(dir, 'main.c.v')
	os.write_file(source, '#include "${header}"
fn C.sum_record_drop(code int, address voidptr)
fn C.sum_drop_count(code int) int
fn C.sum_last_drop(code int) voidptr
struct Item { code int }
fn (item &Item) free() { C.sum_record_drop(item.code, voidptr(item)) }
fn (item &Item) drop() { C.sum_record_drop(item.code, voidptr(item)) }
type Choice = int | &Item
type ItemPointer = &Item
type AliasPointer = ItemPointer
type AliasChoice = int | AliasPointer
type ValueChoice = int | Item
type ScalarChoice = bool | int
struct Empty {}
type EmptyChoice = Empty | int
struct Holder { item &Item }
fn borrowed(item &Item) &Item { return item }
fn fresh(code int) Choice { return Choice(&Item{code: code}) }
fn mixed(owned bool, item &Item) Choice {
 if owned { return Choice(&Item{code: 4}) }
 return Choice(item)
}
fn original_reassignment_after_move() {
 mut original := Choice(&Item{code: 1})
 address := voidptr(original as &Item)
 copy := original
 original = Choice(0)
 assert original is int
 assert voidptr(copy as &Item) == address
 assert C.sum_drop_count(1) == 0
}
fn copy_reassignment_after_move() {
 original := fresh(2)
 address := voidptr(original as &Item)
 mut copy := original
 copy = Choice(0)
 assert copy is int
 assert C.sum_drop_count(2) == 1
 assert C.sum_last_drop(2) == address
}
fn borrowed_copies(item &Item) {
 value := Choice(item)
 copy := value
 another := value
 assert voidptr(value as &Item) == voidptr(item)
 assert voidptr(copy as &Item) == voidptr(item)
 assert voidptr(another as &Item) == voidptr(item)
 holder := Holder{item: item}
 projected := Choice(holder.item)
 projected_copy := projected
 assert voidptr(projected as &Item) == voidptr(projected_copy as &Item)
 returned := Choice(borrowed(item))
 returned_copy := returned
 assert voidptr(returned as &Item) == voidptr(returned_copy as &Item)
 mut replacement := returned
 replacement = Choice(0)
 assert replacement is int
 assert (value as &Item).code == 3
 assert C.sum_drop_count(3) == 0
}
fn conditional(owned bool, item &Item) {
 value := if owned { Choice(&Item{code: 5}) } else { Choice(item) }
 assert (value as &Item).code == if owned { 5 } else { 3 }
}
fn borrowed_scope() {
 local := Item{code: 3}
 borrowed_copies(&local)
 from_call := mixed(false, &local)
 assert voidptr(from_call as &Item) == voidptr(&local)
 conditional(false, &local)
 assert C.sum_drop_count(3) == 0
 from_owned_call := mixed(true, &local)
 assert (from_owned_call as &Item).code == 4
 conditional(true, &local)
 assert C.sum_drop_count(5) == 1
}
fn aliased_scope() {
 value := AliasChoice(AliasPointer(ItemPointer(&Item{code: 6})))
 assert (value as AliasPointer).code == 6
}
fn owned_value_scope() {
 original := Item{code: 7}
 value := ValueChoice(original)
 assert (value as Item).code == 7
}
fn scalar_copies() {
 value := ScalarChoice(9)
 copy := value
 another := value
 assert (value as int) == 9
 assert (copy as int) == 9
 assert (another as int) == 9
 empty := EmptyChoice(Empty{})
 empty_copy := empty
 another_empty := empty
 assert empty is Empty
 assert empty_copy is Empty
 assert another_empty is Empty
}
fn main() {
 scalar_copies()
 original_reassignment_after_move()
 assert C.sum_drop_count(1) == 1
 copy_reassignment_after_move()
 assert C.sum_drop_count(2) == 1
 borrowed_scope()
 assert C.sum_drop_count(3) == 1
 assert C.sum_drop_count(4) == 1
 assert C.sum_drop_count(5) == 1
 aliased_scope()
 assert C.sum_drop_count(6) == 1
 owned_value_scope()
 assert C.sum_drop_count(7) == 1
}
')!
	for mode in ['-autofree', '-ownership'] {
		for prealloc in [false, true] {
			mut command := [compiler, '-new-compiler', '-nocache', '-no-closures', '-gc', 'none',
				'-cc', cc, mode]
			if prealloc { command << '-prealloc' }
			command << ['run', source]
			result := os.exec(command)
			assert result.exit_code == 0, '${mode}, prealloc=${prealloc}: ${result.output}'
		}
	}
}
