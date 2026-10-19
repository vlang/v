module c

import os

fn test_returned_pointer_sum_acquires_promoted_local_once() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'sum_local_transfer_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	compiler := os.join_path(dir, 'v-ownership')
	vlib := os.join_path(@VEXEROOT, 'vlib')
	build := os.exec([@VEXE, '-new-compiler', '-b', 'c', '-cc', cc, '-gc', 'none', '-d', 'ownership',
		'-path', vlib, '-o', compiler, os.join_path(vlib, 'v', 'v.v')])
	assert build.exit_code == 0, build.output
	header := os.join_path(dir, 'drops.h')
	os.write_file(header, 'static int transfer_drops[16];
static void transfer_record(int code) { transfer_drops[code]++; }
static int transfer_count(int code) { return transfer_drops[code]; }
')!
	path := os.join_path(dir, 'main.c.v')
	os.write_file(path, '#include "${header}"
fn C.transfer_record(code int)
fn C.transfer_count(code int) int
@[aligned: 64]
struct Item implements Drop { code int }
fn (item &Item) free() { C.transfer_record(item.code) }
fn (mut item Item) drop() { C.transfer_record(item.code) }
type Choice = int | &Item
struct Holder { item &Item }
fn borrow(item &Item) &Item { return item }
@[noinline]
fn ordinary_factory() Choice {
 local := Item{code: 1}
 first := Choice(&local)
 second := Choice(&local)
 alias := first
 assert voidptr(second as &Item) == voidptr(alias as &Item)
 assert voidptr(alias as &Item) == voidptr(&local)
 return alias
}
@[manualfree; noinline]
fn manual_factory() Choice {
 local := Item{code: 2}
 first := Choice(&local)
 alias := first
 assert voidptr(alias as &Item) == voidptr(&local)
 return alias
}
@[noinline]
fn direct_factory() Choice {
 local := Item{code: 3}
 return Choice(&local)
}
@[noinline]
fn borrowed_call_local_factory() Choice {
 local := Item{code: 13}
 return Choice(borrow(&local))
}
@[noinline]
fn nested_factory(flag bool) Choice {
 mut owner := Choice(0)
 mut result := Choice(0)
 if flag {
  local := Item{code: 5}
  owner = Choice(&local)
  result = Choice(&local)
  assert voidptr(result as &Item) == voidptr(&local)
  assert voidptr(owner as &Item) == voidptr(result as &Item)
 }
 assert C.transfer_count(5) == 0
 return result
}
@[noinline]
fn nested_alias_factory() Choice {
 mut result := Choice(0)
 if true {
  local := Item{code: 6}
  wrapped := Choice(&local)
  copied := wrapped
  result = copied
  assert voidptr(result as &Item) == voidptr(&local)
 }
 assert C.transfer_count(6) == 0
 return result
}
@[noinline]
fn conditional_factory(flag bool) Choice {
 local := Item{code: 7}
 return if flag { Choice(&local) } else { Choice(0) }
}
@[noinline]
fn match_factory(flag bool) Choice {
 local := Item{code: 8}
 return match flag {
  true { Choice(&local) }
  false { Choice(0) }
 }
}
@[noinline]
fn nested_owner_copy_factory() Choice {
 mut owner := Choice(0)
 if true {
  local := Item{code: 9}
  owner = Choice(&local)
 }
 copied := owner
 alias := copied
 return alias
}
@[noinline]
fn loop_factory() Choice {
 mut owner := Choice(0)
 for code in 10 .. 13 {
  assert C.transfer_count(14) == code - 10
  temporary := Item{code: 14}
  assert temporary.code == 14
  local := Item{code: code}
  wrapped := Choice(&local)
  owner = wrapped
  assert C.transfer_count(code) == 0
 }
 assert C.transfer_count(14) == 3
 assert C.transfer_count(10) == 1
 assert C.transfer_count(11) == 1
 assert C.transfer_count(12) == 0
 return owner
}
@[noinline]
fn loop_scope_cleanup() {
 mut owner := Choice(0)
 for _ in 0 .. 2 {
  local := Item{code: 15}
  owner = Choice(&local)
 }
 assert C.transfer_count(15) == 1
 assert (owner as &Item).code == 15
}
@[noinline]
fn churn_stack() u64 {
 mut words := [512]u64{}
 for i in 0 .. words.len { words[i] = u64(i + 1) * 97 }
 mut value := u64(0)
 for word in words { value ^= word }
 return value
}
@[noinline]
fn exercise_factories() {
 ordinary := ordinary_factory()
 manual := manual_factory()
 direct := direct_factory()
 assert C.transfer_count(1) == 0
 assert C.transfer_count(2) == 0
 assert C.transfer_count(3) == 0
 assert (ordinary as &Item).code == 1
 assert (manual as &Item).code == 2
 assert (direct as &Item).code == 3
 assert usize(voidptr(ordinary as &Item)) % 64 == 0
 nested := nested_factory(true)
 absent := nested_factory(false)
 assert absent is int
 assert churn_stack() != 0
 assert (nested as &Item).code == 5
 assert usize(voidptr(nested as &Item)) % 64 == 0
 assert C.transfer_count(5) == 0
 nested_alias := nested_alias_factory()
 assert (nested_alias as &Item).code == 6
 conditional := conditional_factory(true)
 assert (conditional as &Item).code == 7
 assert C.transfer_count(7) == 0
 matched := match_factory(true)
 assert (matched as &Item).code == 8
 assert C.transfer_count(8) == 0
 copied_owner := nested_owner_copy_factory()
 assert (copied_owner as &Item).code == 9
 looped := loop_factory()
 assert (looped as &Item).code == 12
 through_call := borrowed_call_local_factory()
 assert (through_call as &Item).code == 13
 assert C.transfer_count(13) == 0
}
@[noinline]
fn borrow_variants(item &Item, holder &Holder) {
 first := Choice(item)
 second := first
 call := Choice(borrow(item))
 selected := Choice(holder.item)
 assert voidptr(first as &Item) == voidptr(second as &Item)
 assert voidptr(call as &Item) == voidptr(selected as &Item)
 assert C.transfer_count(4) == 0
}
@[noinline]
fn exercise_borrowed() {
 local := Item{code: 4}
 first := Choice(&local)
 second := first
 holder := Holder{item: &local}
 borrow_variants(&local, &holder)
 assert voidptr(first as &Item) == voidptr(second as &Item)
 assert C.transfer_count(4) == 0
}
fn main() {
 exercise_factories()
 assert C.transfer_count(1) == 1
 assert C.transfer_count(2) == 1
 assert C.transfer_count(3) == 1
 assert C.transfer_count(5) == 1
 assert C.transfer_count(6) == 1
 assert C.transfer_count(7) == 1
 assert C.transfer_count(8) == 1
 assert C.transfer_count(9) == 1
 assert C.transfer_count(12) == 1
 assert C.transfer_count(13) == 1
 absent := conditional_factory(false)
 assert absent is int
 assert C.transfer_count(7) == 2
 unmatched := match_factory(false)
 assert unmatched is int
 assert C.transfer_count(8) == 2
 exercise_borrowed()
 assert C.transfer_count(4) == 1
 loop_scope_cleanup()
 assert C.transfer_count(15) == 2
}')!
	for mode in ['-autofree', '-ownership'] {
		for prealloc in [false, true] {
			mut command := [compiler, '-new-compiler', '-nocache', '-no-closures', '-gc', 'none',
				'-cc', cc, mode]
			if prealloc { command << '-prealloc' }
			command << ['run', path]
			result := os.exec(command)
			assert result.exit_code == 0, '${mode}, prealloc=${prealloc}: ${result.output}'
		}
	}
	invalid := os.join_path(dir, 'ambiguous.v')
	os.write_file(invalid, 'struct Item { code int }
type Choice = int | &Item
fn factory(flag bool, item &Item) Choice {
 mut result := Choice(0)
 if true {
  local := Item{code: 1}
  wrapped := if flag { Choice(&local) } else { Choice(item) }
  result = wrapped
 }
 return result
}
fn main() { item := Item{code: 8}; value := factory(true, &item); assert value is &Item }')!
	for mode in ['-autofree', '-ownership'] {
		result := os.exec([compiler, '-new-compiler', '-no-closures', '-gc', 'none', mode, '-check',
			invalid])
		assert result.exit_code != 0
		assert result.output.contains('ambiguous local pointer source outside its scope'), result.output
	}
	unsupported := {
		'struct Item { code int }
type Choice = int | &Item
fn consume(flag bool, item &Item) {
 mut result := Choice(0)
 if true {
  local := Item{code: 1}
  wrapped := if flag { Choice(&local) } else { Choice(item) }
  result = wrapped
 }
 assert result is int || result is &Item
}
fn main() { item := Item{code: 8}; consume(true, &item) }':                                                                                                        'ambiguous local pointer source outside its scope'
		'struct Item { code int }
type Choice = int | &Item
fn borrow(item &Item) &Item { return item }
fn factory() Choice {
 mut result := Choice(0)
 if true {
  local := Item{code: 1}
  result = Choice(borrow(&local))
 }
 return result
}
fn main() { value := factory(); assert value is &Item }':                                                                                                                                                     'ambiguous local pointer source outside its scope'
		'struct Item { code int }
type Choice = int | &Item
fn factory(flag bool) Choice {
 return if flag {
  local := Item{code: 1}
  Choice(&local)
 } else { Choice(0) }
}
fn main() { value := factory(true); assert value is &Item }':                                                                                                                                                                                                                   'local declared inside an if or match expression'
		'struct Item { code int }
type Choice = int | &Item
fn factory(flag bool, item &Item) Choice {
 return Choice(if flag { local := Item{code: 5}; &local } else { item })
}
fn main() { item := Item{code: 8}; value := factory(true, &item); assert value is &Item }':                                                                                                                                                                                  'local declared inside an if or match expression'
		'struct Item implements Drop { code int }
fn (item &Item) free() {}
fn (mut item Item) drop() {}
type Choice = int | &Item
fn factory() Choice {
 mut owner := Choice(0)
 mut loan := Choice(0)
 for _ in 0 .. 1 {
  local := Item{code: 1}
  owner = Choice(&local)
  loan = Choice(&local)
 }
 assert loan is &Item
 return owner
}
fn main() { value := factory(); assert value is &Item }':                                                        'borrowed by `loan`'
		'struct Item implements Drop { code int }
fn (item &Item) free() {}
fn (mut item Item) drop() {}
type Choice = int | &Item
fn factory() Choice {
 mut owner := Choice(0)
 mut loan := Choice(0)
 if true {
  local := Item{code: 1}
  owner = Choice(&local)
  loan = Choice(&local)
 }
 for _ in 0 .. 1 {
  loan := Choice(0)
  assert loan is int
 }
 assert loan is &Item
 return owner
}
fn main() { value := factory(); assert value is &Item }': 'borrowed by `loan`'
	}
	for source, expected in unsupported {
		os.write_file(invalid, source)!
		for mode in ['-autofree', '-ownership'] {
			result := os.exec([compiler, '-new-compiler', '-no-closures', '-gc', 'none', mode,
				'-check', invalid])
			assert result.exit_code != 0
			assert result.output.contains(expected), result.output
		}
	}
}
