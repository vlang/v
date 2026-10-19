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
import strconv
fn C.sum_record_drop(code int, address voidptr)
fn C.sum_drop_count(code int) int
fn C.sum_last_drop(code int) voidptr
interface Drop { mut: drop() }
struct Item implements Drop { code int }
fn (item &Item) free() { C.sum_record_drop(item.code, voidptr(item)) }
fn (mut item Item) drop() { C.sum_record_drop(item.code, voidptr(&item)) }
type Choice = int | &Item
type ItemPointer = &Item
type AliasPointer = ItemPointer
type AliasChoice = int | AliasPointer
type ValueChoice = int | Item
type ScalarChoice = bool | int
struct Empty {}
type EmptyChoice = Empty | int
struct Holder { item &Item }
struct SumHolder { value Choice }
fn borrowed(item &Item) &Item { return item }
fn borrowed_sum(item &Item) Choice { return Choice(item) }
fn fresh(code int) Choice { return Choice(&Item{code: code}) }
fn mixed(owned bool, item &Item) Choice {
 if owned { return Choice(&Item{code: 4}) }
 return Choice(item)
}
fn mixed_code(owned bool, item &Item, code int) Choice {
 if owned { return Choice(&Item{code: code}) }
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
fn borrowed_copies(item &Item, holder &Holder) {
 value := Choice(item)
 copy := value
 another := value
 assert voidptr(value as &Item) == voidptr(item)
 assert voidptr(copy as &Item) == voidptr(item)
 assert voidptr(another as &Item) == voidptr(item)
 projected := Choice(holder.item)
 projected_copy := projected
 assert voidptr(projected as &Item) == voidptr(projected_copy as &Item)
 returned := Choice(borrowed(item))
 returned_copy := returned
 assert voidptr(returned as &Item) == voidptr(returned_copy as &Item)
 borrowed_result := borrowed_sum(item)
 result_copy := borrowed_result
 another_result := borrowed_result
 assert voidptr(borrowed_result as &Item) == voidptr(item)
 assert voidptr(result_copy as &Item) == voidptr(item)
 assert voidptr(another_result as &Item) == voidptr(item)
 wrapped_result := Choice(borrowed_sum(item))
 wrapped_copy := Choice(wrapped_result)
 another_wrapped := wrapped_result
 assert voidptr(wrapped_result as &Item) == voidptr(item)
 assert voidptr(wrapped_copy as &Item) == voidptr(item)
 assert voidptr(another_wrapped as &Item) == voidptr(item)
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
fn mixed_storage_scope(item &Item) {
 source := SumHolder{value: Choice(mixed_code(true, item, 14))}
 field_alias := Choice(source.value)
 assert (field_alias as &Item).code == 14
 assert C.sum_drop_count(14) == 0
 values := [Choice(mixed_code(true, item, 15))]
 index_alias := Choice(values[0])
 assert (index_alias as &Item).code == 15
 assert C.sum_drop_count(15) == 0
}
fn borrowed_scope() {
 local := Item{code: 3}
 holder := Holder{item: &local}
 borrowed_copies(&local, &holder)
 from_call := mixed(false, &local)
 assert voidptr(from_call as &Item) == voidptr(&local)
 borrowed_alias := from_call
 assert voidptr(borrowed_alias as &Item) == voidptr(&local)
 wrapped_borrow := Choice(Choice(mixed(false, &local)))
 wrapped_alias := Choice(wrapped_borrow)
 assert voidptr(wrapped_alias as &Item) == voidptr(&local)
 conditional(false, &local)
 assert C.sum_drop_count(3) == 0
 from_owned_call := mixed(true, &local)
 assert (from_owned_call as &Item).code == 4
 owned_alias := from_owned_call
 assert (owned_alias as &Item).code == 4
 conditional_local := if local.code == 3 { Choice(&Item{code: 8}) } else { Choice(&local) }
 conditional_alias := conditional_local
 assert (conditional_alias as &Item).code == 8
 borrowed_local := if local.code == 0 { Choice(&Item{code: 9}) } else { Choice(&local) }
 local_alias := borrowed_local
 assert voidptr(local_alias as &Item) == voidptr(&local)
 wrapped_owned := Choice(mixed_code(true, &local, 10))
 wrapped_owned_alias := Choice(wrapped_owned)
 assert (wrapped_owned_alias as &Item).code == 10
 wrapped_conditional := Choice(if local.code == 3 { mixed_code(true, &local, 11) } else { Choice(&local) })
 conditional_copy := Choice(wrapped_conditional)
 assert (conditional_copy as &Item).code == 11
 pure_fresh := Choice(Choice(fresh(12)))
 fresh_alias := Choice(pure_fresh)
 assert (fresh_alias as &Item).code == 12
 wrapped_match := Choice(match local.code { 3 { mixed_code(true, &local, 13) } else { Choice(&local) } })
 match_copy := wrapped_match
 assert (match_copy as &Item).code == 13
 mixed_storage_scope(&local)
 assert C.sum_drop_count(14) == 1
 assert C.sum_drop_count(15) == 1
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
 parsed := strconv.atoi("12") or { panic(err) }
 floating := strconv.atof64("1.5") or { panic(err) }
 assert parsed == 12
 assert floating == 1.5
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
 assert C.sum_drop_count(8) == 1
 assert C.sum_drop_count(9) == 0
 assert C.sum_drop_count(10) == 1
 assert C.sum_drop_count(11) == 1
 assert C.sum_drop_count(12) == 1
 assert C.sum_drop_count(13) == 1
 assert C.sum_drop_count(14) == 1
 assert C.sum_drop_count(15) == 1
}
')!
	// A method named drop is ordinary user code unless the type implements Drop.
	// Keep it unreachable so a spurious destructor also exposes missing C retention.
	ordinary_source := os.join_path(dir, 'ordinary_drop.c.v')
	os.write_file(ordinary_source, '#include "${header}"
fn C.sum_record_drop(code int, address voidptr)
fn C.sum_drop_count(code int) int
struct Item { code int }
fn (mut item Item) drop() { C.sum_record_drop(item.code, voidptr(&item)) }
type Choice = int | &Item
fn ordinary_scope() {
 mut value := Choice(&Item{code: 8})
 value = Choice(0)
 assert value is int
 assert C.sum_drop_count(8) == 0
 mut original := Choice(&Item{code: 9})
 copy := original
 original = Choice(0)
 assert original is int
 assert (copy as &Item).code == 9
 assert C.sum_drop_count(9) == 0
 plain := Item{code: 10}
 plain_copy := plain
 assert plain.code == 10
 assert plain_copy.code == 10
}
fn main() {
 ordinary_scope()
 assert C.sum_drop_count(8) == 0
 assert C.sum_drop_count(9) == 0
 assert C.sum_drop_count(10) == 0
}
')!
	or_source := os.join_path(dir, 'or_owners.c.v')
	os.write_file(or_source, '#include "${header}"
fn C.sum_record_drop(code int, address voidptr)
fn C.sum_drop_count(code int) int
interface Drop { mut: drop() }
struct Item implements Drop { code int }
fn (item &Item) free() { C.sum_record_drop(item.code, voidptr(item)) }
fn (mut item Item) drop() { C.sum_record_drop(item.code, voidptr(&item)) }
type Choice = int | &Item
fn result_value(mode int, item &Item, code int) !Choice {
 if mode == 0 { return error("fallback") }
 if mode == 1 { return Choice(&Item{code: code}) }
 return Choice(item)
}
fn option_value(mode int, item &Item, code int) ?Choice {
 if mode == 0 { return none }
 if mode == 1 { return Choice(&Item{code: code}) }
 return Choice(item)
}
fn borrowed_result(item &Item) !Choice { return Choice(item) }
fn borrowed_option(item &Item) ?Choice { return Choice(item) }
fn maybe_fresh(owned bool, code int) !Choice {
 if owned { return Choice(&Item{code: code}) }
 return error("absent")
}
fn cleared_alias_scope() {
 mut local := Item{code: 9}
 value := maybe_fresh(false, 12) or {
  mut inner := Choice(0)
  for i in 0 .. 2 {
   iteration := Choice(&local)
   assert voidptr(iteration as &Item) == voidptr(&local)
   inner = iteration
   assert i < 2
  }
  inner = Choice(0)
  inner
 }
 alias := Choice(value)
 local = Item{code: 10}
 assert alias is int
 assert C.sum_drop_count(9) == 1
}
fn cleared_exits_scope() {
 mut local := Item{code: 12}
 broken := maybe_fresh(false, 14) or {
  mut inner := Choice(0)
  for i in 0 .. 2 {
   if local.code == 12 { inner = Choice(&local); break }
   inner = Choice(0)
   assert i < 2
  }
  inner = Choice(0)
  inner
 }
 continued := maybe_fresh(false, 14) or {
  mut inner := Choice(0)
  for i in 0 .. 2 {
   if local.code == 12 { continue }
   inner = Choice(&local)
   inner = Choice(0)
   assert i < 2
  }
  inner
 }
 broken_alias := Choice(broken)
 continued_alias := Choice(continued)
 local = Item{code: 13}
 assert broken_alias is int
 assert continued_alias is int
 assert C.sum_drop_count(12) == 1
}
fn result_success(item &Item) ! {
 value := result_value(1, item, 1)!
 alias := Choice(value)
 assert (alias as &Item).code == 1
 assert C.sum_drop_count(1) == 0
}
fn option_success(item &Item) ? {
 value := option_value(1, item, 2)?
 alias := Choice(value)
 assert (alias as &Item).code == 2
 assert C.sum_drop_count(2) == 0
}
fn result_fallback(item &Item) {
 value := Choice(result_value(0, item, 3) or { Choice(&Item{code: 3}) })
 alias := value
 assert (alias as &Item).code == 3
 assert C.sum_drop_count(3) == 0
}
fn option_fallback(item &Item) {
 fallback := Choice(&Item{code: 4})
 value := option_value(0, item, 4) or { fallback }
 alias := Choice(value)
 assert (alias as &Item).code == 4
 assert C.sum_drop_count(4) == 0
}
fn forward_scope(item &Item) {
 result := forward_result(item) or { panic(err) }
 assert (result as &Item).code == 5
 option := forward_option(item) or { panic("none") }
 assert (option as &Item).code == 6
 fallback := forward_fallback(item) or { panic(err) }
 assert (fallback as &Item).code == 11
 assert C.sum_drop_count(5) == 0
 assert C.sum_drop_count(6) == 0
 assert C.sum_drop_count(11) == 0
}
fn forward_fallback(item &Item) !Choice {
 owned := Choice(&Item{code: 11})
 return Choice(result_value(0, item, 12) or { owned })
}
fn forward_result(item &Item) !Choice {
 value := result_value(1, item, 5)!
 return value
}
fn forward_option(item &Item) ?Choice {
 value := option_value(1, item, 6)?
 return value
}
fn borrowed_controls(item &Item) ! {
 mixed := result_value(2, item, 8)!
 mixed_alias := Choice(mixed)
 assert voidptr(mixed_alias as &Item) == voidptr(item)
 result := borrowed_result(item)!
 result_copy := result
 another_result := Choice(result)
 assert voidptr(result as &Item) == voidptr(item)
 assert voidptr(result_copy as &Item) == voidptr(item)
 assert voidptr(another_result as &Item) == voidptr(item)
 option := borrowed_option(item) or { Choice(item) }
 option_copy := Choice(option)
 another_option := option
 assert voidptr(option as &Item) == voidptr(item)
 assert voidptr(option_copy as &Item) == voidptr(item)
 assert voidptr(another_option as &Item) == voidptr(item)
 assert C.sum_drop_count(7) == 0
 assert C.sum_drop_count(8) == 0
}
fn main() {
 local := Item{code: 7}
 result_success(&local) or { panic(err) }
 assert C.sum_drop_count(1) == 1
 option_success(&local) or { panic("none") }
 assert C.sum_drop_count(2) == 1
 result_fallback(&local)
 assert C.sum_drop_count(3) == 1
 option_fallback(&local)
 assert C.sum_drop_count(4) == 1
 forward_scope(&local)
 assert C.sum_drop_count(5) == 1
 assert C.sum_drop_count(6) == 1
 borrowed_controls(&local) or { panic(err) }
 assert C.sum_drop_count(7) == 0
 assert C.sum_drop_count(8) == 0
 cleared_alias_scope()
 assert C.sum_drop_count(9) == 1
 assert C.sum_drop_count(10) == 1
 assert C.sum_drop_count(11) == 1
 cleared_exits_scope()
 assert C.sum_drop_count(12) == 1
 assert C.sum_drop_count(13) == 1
}
')!
	previous_jobs := os.getenv('VJOBS')
	defer {
		if previous_jobs.len > 0 {
			os.setenv('VJOBS', previous_jobs, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	for jobs in ['1', '3'] {
		os.setenv('VJOBS', jobs, true)
		for mode in ['-autofree', '-ownership'] {
			for prealloc in [false, true] {
				for program in [source, ordinary_source, or_source] {
					mut command := [compiler, '-new-compiler', '-nocache', '-no-closures', '-gc',
						'none', '-cc', cc, mode]
					if prealloc { command << '-prealloc' }
					command << ['run', program]
					result := os.exec(command)
					assert result.exit_code == 0, '${os.base(program)}, ${mode}, jobs=${jobs}, prealloc=${prealloc}: ${result.output}'
				}
			}
		}
	}
}
