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
	os.write_file(header, 'static int transfer_drops[35];
static void transfer_record(int code) { transfer_drops[code]++; }
static int transfer_count(int code) { return transfer_drops[code]; }
static int transfer_visits;
static int transfer_index(void) { transfer_visits++; return 0; }
static int transfer_visit_count(void) { return transfer_visits; }
')!
	path := os.join_path(dir, 'main.c.v')
	os.write_file(path, '#include "${header}"
fn C.transfer_record(code int)
fn C.transfer_count(code int) int
fn C.transfer_index() int
fn C.transfer_visit_count() int
@[aligned: 64]
struct Item implements Drop { code int }
fn (item &Item) free() { C.transfer_record(item.code) }
fn (mut item Item) drop() { C.transfer_record(item.code) }
type Choice = int | &Item
struct Holder { item &Item }
struct SumHolder { mut: value Choice }
struct MixedSumHolder { borrowed Choice; owned Choice }
fn borrow(item &Item) &Item { return item }
fn result_borrow(item &Item) !Choice { return Choice(item) }
fn option_borrow(item &Item) ?Choice { return Choice(item) }
fn or_local_return_cleanup() {
 result := result_local_factory()
 option := option_local_factory()
 assert (result as &Item).code == 33
 assert (option as &Item).code == 34
 assert C.transfer_count(33) == 0
 assert C.transfer_count(34) == 0
}
fn result_local_factory() Choice {
 local := Item{code: 33}
 value := result_borrow(&local) or { Choice(0) }
 assert voidptr(value as &Item) == voidptr(&local)
 return value
}
fn option_local_factory() Choice {
 local := Item{code: 34}
 value := option_borrow(&local) or { Choice(0) }
 assert voidptr(value as &Item) == voidptr(&local)
 return value
}
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
fn projection_scope_cleanup() {
 mut holder := SumHolder{value: Choice(&Item{code: 16})}
 for i in 0 .. 2 {
  holder.value = Choice(&Item{code: 17})
  assert C.transfer_count(16) == 1
  assert C.transfer_count(17) == i
 }
 pointer := holder.value as &Item
 holder.value = Choice(pointer)
 holder.value = holder.value
 assert pointer.code == 17
 assert C.transfer_count(17) == 1
 local := Item{code: 18}
 holder.value = Choice(&local)
 assert voidptr(holder.value as &Item) == voidptr(&local)
 assert C.transfer_count(17) == 2
 assert C.transfer_count(18) == 0
 mut values := [Choice(&Item{code: 19})]
 values[C.transfer_index()] = Choice(&Item{code: 20})
 assert C.transfer_visit_count() == 1
 assert C.transfer_count(19) == 1
 element := values[0] as &Item
 values[0] = Choice(element)
 values[0] = values[0]
 assert element.code == 20
 assert C.transfer_count(20) == 0
 mut holders := [SumHolder{value: Choice(0)}]
 holders[C.transfer_index()].value = Choice(&Item{code: 21})
 assert C.transfer_visit_count() == 2
 assert C.transfer_count(21) == 0
 mut source := SumHolder{value: Choice(&Item{code: 24})}
 mut destination := SumHolder{value: Choice(&Item{code: 25})}
 same := source.value as &Item
 source.value = Choice(same)
 source.value = source.value
 destination.value = source.value
 assert C.transfer_count(25) == 1
 assert C.transfer_count(24) == 0
 assert (destination.value as &Item).code == 24
}
@[noinline]
fn forward_loop_cleanup() {
 first := forward_range_loop_factory()
 first_alias := first
 assert (first_alias as &Item).code == 22
 assert C.transfer_count(22) == 0
 second := forward_c_loop_factory()
 second_alias := second
 assert (second_alias as &Item).code == 23
 assert C.transfer_count(23) == 0
}
@[noinline]
fn forward_projection_cleanup() {
 field := forward_field_factory()
 assert (field as &Item).code == 29
 assert C.transfer_count(29) == 0
 assert C.transfer_count(31) == 1
 indexed := forward_index_factory()
 assert (indexed as &Item).code == 30
 assert C.transfer_count(30) == 0
 assert C.transfer_count(32) == 1
}
@[noinline]
fn forward_field_factory() Choice {
 holder := MixedSumHolder{borrowed: Choice(&Item{code: 31}), owned: Choice(&Item{code: 29})}
 return holder.owned
}
@[noinline]
fn forward_index_factory() Choice {
 values := [Choice(&Item{code: 30}), Choice(&Item{code: 32})]
 return values[0]
}
@[noinline]
fn aggregate_borrow_cleanup() {
 local := Item{code: 27}
 mut same_scope := SumHolder{value: Choice(0)}
 same_scope = SumHolder{value: Choice(&local)}
 assert voidptr(same_scope.value as &Item) == voidptr(&local)
 mixed := MixedSumHolder{borrowed: Choice(&local), owned: Choice(&Item{code: 26})}
 alias := mixed
 moved := alias
 assert voidptr(moved.borrowed as &Item) == voidptr(&local)
 assert (moved.owned as &Item).code == 26
 values := [SumHolder{value: Choice(&local)}]
 array_alias := values
 assert voidptr(array_alias[0].value as &Item) == voidptr(&local)
 flag := local.code == 27
 conditional := if flag { SumHolder{value: Choice(&local)} } else { SumHolder{value: Choice(0)} }
 assert voidptr(conditional.value as &Item) == voidptr(&local)
 fresh := if flag { SumHolder{value: Choice(&Item{code: 28})} } else { SumHolder{value: Choice(0)} }
 assert (fresh.value as &Item).code == 28
 assert C.transfer_count(26) == 0
 assert C.transfer_count(27) == 0
}
@[noinline]
fn forward_range_loop_factory() Choice {
 mut owner := Choice(0)
 for _ in 0 .. 1 {
  local := Item{code: 22}
  wrapped := Choice(&local)
  owner = wrapped
 }
 return owner
}
@[noinline]
fn forward_c_loop_factory() Choice {
 mut owner := Choice(0)
 for i := 0; i < 1; i++ {
  local := Item{code: 23}
  wrapped := Choice(&local)
  owner = wrapped
 }
 return owner
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
 assert uint(voidptr(ordinary as &Item)) % 64 == 0
 nested := nested_factory(true)
 absent := nested_factory(false)
 assert absent is int
 assert churn_stack() != 0
 assert (nested as &Item).code == 5
 assert uint(voidptr(nested as &Item)) % 64 == 0
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
 projection_scope_cleanup()
 assert C.transfer_count(16) == 1
 assert C.transfer_count(17) == 2
 assert C.transfer_count(18) == 1
 assert C.transfer_count(19) == 1
 assert C.transfer_count(20) == 1
 assert C.transfer_count(21) == 1
 forward_loop_cleanup()
 assert C.transfer_count(22) == 1
 assert C.transfer_count(23) == 1
 assert C.transfer_count(24) == 1
 assert C.transfer_count(25) == 1
 aggregate_borrow_cleanup()
 assert C.transfer_count(26) == 1
 assert C.transfer_count(27) == 1
 assert C.transfer_count(28) == 1
 forward_projection_cleanup()
 assert C.transfer_count(29) == 1
 assert C.transfer_count(30) == 1
 assert C.transfer_count(31) == 1
 assert C.transfer_count(32) == 1
 or_local_return_cleanup()
 assert C.transfer_count(33) == 1
 assert C.transfer_count(34) == 1
}')!
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
				mut command := [compiler, '-new-compiler', '-nocache', '-no-closures', '-gc', 'none',
					'-cc', cc, mode]
				if prealloc { command << '-prealloc' }
				command << ['run', path]
				result := os.exec(command)
				assert result.exit_code == 0, '${mode}, jobs=${jobs}, prealloc=${prealloc}: ${result.output}'
			}
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
		'struct Item { code int }
type Choice = int | &Item
fn absent() !Choice { return error("absent") }
fn factory() Choice {
 return absent() or { local := Item{code: 1}; Choice(&local) }
}
fn main() { value := factory(); assert value is &Item }':                                                                                                                                                                                                    '`or` block'
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
		'struct Item { code int }
type Choice = int | &Item
struct Inner { mut: value Choice }
struct Holder { mut: inner Inner }
fn main() {
 mut holder := Holder{inner: Inner{value: Choice(0)}}
 if true {
  local := Item{code: 1}
  wrapped := Choice(&local)
  holder.inner.value = wrapped
 }
 assert holder.inner.value is &Item
}':                                                                                                                  'shorter lifetime in aggregate storage'
		'struct Item { code int }
type Choice = int | &Item
fn main() {
 mut values := [Choice(0)]
 if true {
  local := Item{code: 1}
  values[0] = Choice(&local)
 }
 assert values[0] is &Item
}':                                                                                                                                                                                                                                                          'shorter lifetime in aggregate storage'
		'struct Item { code int }
type Choice = int | &Item
struct Holder { mut: value Choice }
fn factory() Choice {
 local := Item{code: 1}
 mut holder := Holder{value: Choice(0)}
 holder.value = Choice(&local)
 holder.value = holder.value
 alias := holder.value
 return alias
}
fn main() { value := factory(); assert value is &Item }':                                                                                                             'cannot return a sum field or element borrowing local storage'
		'struct Item { code int }
type Choice = int | &Item
fn factory() Choice {
 local := Item{code: 1}
 mut values := [Choice(0)]
 values[0] = Choice(&local)
 return values[0]
}
fn main() { value := factory(); assert value is &Item }':                                                                                                                                                                                                                 'cannot return a sum field or element borrowing local storage'
		'struct Item { code int }
type Choice = int | &Item
struct Holder { mut: value Choice }
fn consume(flag bool, item &Item) {
 mut holder := Holder{value: Choice(0)}
 holder.value = if flag {
  local := Item{code: 1}
  Choice(&local)
 } else { Choice(item) }
 assert holder.value is &Item
}
fn main() { item := Item{code: 8}; consume(true, &item) }':                                                                                           'shorter lifetime in aggregate storage'
	}
	aggregate_returns := ['return Holder{value: Choice(&local)}',
		'holder := Holder{value: Choice(&local)}; return holder']
	for returned in aggregate_returns {
		source := 'struct Item { code int }
type Choice = int | &Item
struct Holder { value Choice }
fn factory() Holder {
 local := Item{code: 1}
 ${returned}
}
fn main() { value := factory(); assert value.value is &Item }'
		os.write_file(invalid, source)!
		for mode in ['-autofree', '-ownership'] {
			result := os.exec([compiler, '-new-compiler', '-no-closures', '-gc', 'none', mode,
				'-check', invalid])
			assert result.exit_code != 0
			assert result.output.contains('cannot return a sum field or element borrowing local storage'), result.output
		}
	}
	aggregate_stores := [
		'struct Item { code int }
type Choice = int | &Item
struct Holder { mut: value Choice }
fn main() {
 mut outer := Holder{value: Choice(0)}
 if true {
  local := Item{code: 1}
  outer = Holder{value: Choice(&local)}
 }
 assert (outer.value as &Item).code == 1
}',
		'struct Item { code int }
type Choice = int | &Item
struct Holder { value Choice }
struct Nested { mut: inner Holder }
fn main() {
 mut outer := Nested{inner: Holder{value: Choice(0)}}
 if true {
  local := Item{code: 1}
  outer.inner = Holder{value: Choice(&local)}
 }
 assert outer.inner.value is &Item
}',
		'struct Item { code int }
type Choice = int | &Item
struct Holder { value Choice }
fn main() {
 mut outer := [Holder{value: Choice(0)}]
 if true {
  local := Item{code: 1}
  outer[0] = Holder{value: Choice(&local)}
 }
 assert outer[0].value is &Item
}',
		'struct Item { code int }
type Choice = int | &Item
fn main() {
 mut outer := [Choice(0)]
 if true {
  local := Item{code: 1}
  outer = [Choice(&local)]
 }
 assert outer[0] is &Item
}',
		'struct Item { code int }
type Choice = int | &Item
struct Holder { value Choice }
fn main() {
 mut outer := Holder{value: Choice(0)}
 if true {
  local := Item{code: 1}
  holder := Holder{value: Choice(&local)}
  alias := holder
  outer = alias
 }
 assert outer.value is &Item
}',
		'struct Item { code int }
type Choice = int | &Item
struct Holder { value Choice }
struct Nested { inner Holder }
fn main() {
 mut outer := Nested{inner: Holder{value: Choice(0)}}
 if true {
  local := Item{code: 1}
  holder := Holder{value: Choice(&local)}
  nested := Nested{inner: holder}
  alias := nested
  outer = alias
 }
 assert outer.inner.value is &Item
}',
		'struct Item { code int }
type Choice = int | &Item
struct Holder { value Choice }
fn consume(flag bool) {
 mut outer := Holder{value: Choice(0)}
 outer = if flag {
  local := Item{code: 1}
  holder := Holder{value: Choice(&local)}
  holder
 } else { Holder{value: Choice(0)} }
 assert outer.value is &Item
}
fn main() { consume(true) }',
	]
	for source in aggregate_stores {
		os.write_file(invalid, source)!
		for mode in ['-autofree', '-ownership'] {
			result := os.exec([compiler, '-new-compiler', '-no-closures', '-gc', 'none', mode,
				'-check', invalid])
			assert result.exit_code != 0
			assert result.output.contains('shorter lifetime in aggregate storage'), result.output
		}
	}
	mixed_loan := 'struct Item implements Drop { code int }
fn (mut item Item) drop() {}
type Choice = int | &Item
fn maybe(owned bool, item &Item) Choice {
 if owned { return Choice(&Item{code: 2}) }
 return Choice(item)
}
fn main() {
 mut local := Item{code: 1}
 value := maybe(false, &local)
 alias := value
 local = Item{code: 3}
 assert alias is &Item
}'
	mut mixed_sources := []string{}
	for initializer in [
		'maybe(false, &local)',
		'Choice(maybe(false, &local))',
		'Choice(Choice(maybe(false, &local)))',
		'Choice(if local.code == 1 { maybe(false, &local) } else { Choice(&local) })',
		'Choice(match local.code { 1 { maybe(false, &local) } else { Choice(&local) } })',
	] {
		mixed_sources << mixed_loan.replace(' value := maybe(false, &local)', ' value := ${initializer}')
	}
	mixed_sources << mixed_loan.replace(' alias := value', ' alias := Choice(value)')
	for wrapper in ['!', '?'] {
		base := mixed_loan.replace(') Choice {', ') ${wrapper}Choice {').replace('fn main() {', 'fn consume() ${wrapper} {')
		for initializer in [
			'maybe(false, &local) or { Choice(&Item{code: 4}) }',
			'Choice(maybe(false, &local) or { Choice(&Item{code: 4}) })',
			'maybe(false, &local)${wrapper}',
			'Choice(maybe(false, &local)${wrapper})',
		] {
			mixed_sources << base.replace(' value := maybe(false, &local)', ' value := ${initializer}') + '\nfn main() { consume() or {} }\n'
		}
	}
	flow_base := mixed_loan.replace(') Choice {', ') !Choice {').replace(' return Choice(item)', ' return error("absent")') + '
fn flow_passthrough(value Choice) Choice { return value }
fn flow_pair(first Choice, second Choice) Choice { assert first is int; return second }
fn flow_bool(value Choice) bool { return value is int }
fn flow_result(value Choice) !Choice { return value }
fn flow_absent_int() !int { return error("absent") }
fn flow_absent_bool() !bool { return error("absent") }
fn flow_absent_values() ![]int { return error("absent") }
struct ScalarBox { value int }
struct FlowState { mut: ready bool }
fn flow_next(mut state FlowState) !bool { if state.ready { return error("absent") }; state.ready = true; return true }
'
	mut flow_sources := []string{}
	for flow in [
		'mut inner := Choice(0); if local.code == 1 { inner = Choice(&local) }; inner',
		'mut inner := Choice(0); match local.code { 1 { inner = Choice(&local) } else {} }; inner',
		'mut inner := Choice(0); for i in 0 .. 1 { inner = Choice(&local); assert i == 0 }; inner',
		'mut inner := Choice(0); for i := 0; i < 1; i++ { inner = Choice(&local) }; inner',
		'mut inner := Choice(0); if local.code == 1 { if local.code > 0 { inner = Choice(&local) } }; inner',
		'mut inner := Choice(0); mut next := Choice(0); for i in 0 .. 2 { inner = next; next = Choice(&local); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { if local.code == 1 { inner = Choice(&local); break }; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { if local.code == 1 { inner = Choice(&local); continue }; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { match local.code { 1 { inner = Choice(&local); break } else {} }; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); outer: for i in 0 .. 2 { for j in 0 .. 2 { if local.code == 1 { inner = Choice(&local); break outer }; inner = Choice(0); assert j < 2 }; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); outer: for i in 0 .. 2 { for j in 0 .. 1 { if local.code == 1 { inner = Choice(&local); continue outer }; inner = Choice(0); assert j == 0 }; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i := 0; i < 2; i++ { if local.code == 1 { inner = Choice(&local); continue }; inner = Choice(0) }; inner',
	] {
		source := flow_base.replace(' value := maybe(false, &local)', ' value := maybe(false, &local) or { ${flow} }')
		flow_sources << source
		mixed_sources << source
	}
	for control in ['break', 'continue'] {
		for evaluation in [
			'Choice(maybe(false, &local) or { inner = Choice(&local); ${control} })',
			'flow_passthrough(maybe(false, &local) or { inner = Choice(&local); ${control} })',
			'flow_pair(maybe(false, &local) or { inner = Choice(&local); ${control} }, maybe(false, &local) or { inner = Choice(0); Choice(0) })',
		] {
			flow := 'mut inner := Choice(0); for i in 0 .. 2 { temporary := ${evaluation}; assert temporary is int; inner = Choice(0); assert i < 2 }; inner'
			source := flow_base.replace(' value := maybe(false, &local)', ' value := maybe(false, &local) or { ${flow} }')
			flow_sources << source
			mixed_sources << source
		}
	}
	for flow in [
		'mut inner := Choice(0); for i in 0 .. 2 { temporary := flow_passthrough(Choice(maybe(false, &local) or { inner = Choice(&local); break })); assert temporary is int; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { if flow_bool(maybe(false, &local) or { inner = Choice(&local); break }) { inner = Choice(0) } else { inner = Choice(0) }; assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { temporary := flow_result(maybe(false, &local) or { inner = Choice(&local); continue }) or { inner = Choice(0); Choice(0) }; assert temporary is int; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { temporary := 1 + (flow_absent_int() or { inner = Choice(&local); break }); assert temporary > 0; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { gate := local.code == 1; if gate && (flow_absent_bool() or { inner = Choice(&local); break }) {}; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(&local); gate := local.code != 1; if gate && (if local.code == 1 { inner = Choice(0); true } else { inner = Choice(0); false }) {}; inner',
		'mut inner := Choice(&local); gate := local.code == 1; if gate || (if local.code == 1 { inner = Choice(0); true } else { inner = Choice(0); false }) {}; inner',
		'mut inner := Choice(0); for outer_i in 0 .. 2 { for item in (flow_absent_values() or { inner = Choice(&local); break }) { assert item >= 0 }; inner = Choice(0); assert outer_i < 2 }; inner',
		'mut inner := Choice(0); for outer_i in 0 .. 2 { for item in (flow_absent_values() or { inner = Choice(&local); continue }) { assert item >= 0 }; inner = Choice(0); assert outer_i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { temporary := [flow_absent_int() or { inner = Choice(&local); break }]; assert temporary.len == 1; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { temporary := ScalarBox{value: (flow_absent_int() or { inner = Choice(&local); break })}; assert temporary.value == 0; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { temporary := {"key": (flow_absent_int() or { inner = Choice(&local); break })}; assert temporary.len == 1; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); for i in 0 .. 2 { temporary := "value:\${flow_absent_int() or { inner = Choice(&local); break }}"; assert temporary.len > 0; inner = Choice(0); assert i < 2 }; inner',
		'mut inner := Choice(0); { outer: for outer_i in 0 .. 2 { for (flow_absent_bool() or { inner = Choice(&local); break outer }) { inner = Choice(0) }; inner = Choice(0); assert outer_i < 2 } }; inner',
		'mut inner := Choice(0); { outer: for outer_i in 0 .. 2 { mut state := FlowState{}; mut next := Choice(0); for (flow_next(mut state) or { inner = next; break outer }) { next = Choice(&local) }; inner = Choice(0); assert outer_i < 2 } }; inner',
	] {
		source := flow_base.replace(' value := maybe(false, &local)', ' value := maybe(false, &local) or { ${flow} }')
		flow_sources << source
		mixed_sources << source
	}
	for source in flow_sources {
		os.write_file(invalid, source.replace(' local = Item{code: 3}\n', ''))!
		for mode in ['-autofree', '-ownership'] {
			for prealloc in [false, true] {
				mut command := [compiler, '-new-compiler', '-no-closures', '-gc', 'none', mode]
				if prealloc { command << '-prealloc' }
				command << ['run', invalid]
				control := os.exec(command)
				assert control.exit_code == 0, control.output
			}
		}
	}
	for source in mixed_sources {
		os.write_file(invalid, source)!
		for mode in ['-autofree', '-ownership'] {
			for prealloc in [false, true] {
				mut command := [compiler, '-new-compiler', '-no-closures', '-gc', 'none', mode]
				if prealloc { command << '-prealloc' }
				command << ['-check', invalid]
				result := os.exec(command)
				assert result.exit_code != 0
				assert result.output.contains('borrowed by `alias`'), result.output
				assert result.output.contains('ambiguous.v:12:'), result.output
			}
		}
	}
	distinct_loan := 'struct Item implements Drop { code int }
fn (mut item Item) drop() {}
type Choice = int | &Item
fn maybe(owned bool, item &Item) !Choice { if owned { return Choice(&Item{code: 4}) }; return Choice(item) }
fn main() {
 mut left := Item{code: 1}
 mut right := Item{code: 2}
 value := maybe(false, &left) or { FALLBACK }
 alias := Choice(value)
 MUTATE
 assert alias is &Item
}'
	for fallback in ['Choice(&right)', 'inner := Choice(&right); inner'] {
		for root in ['left', 'right'] {
			source := distinct_loan.replace('FALLBACK', fallback).replace('MUTATE', '${root} = Item{code: 3}')
			os.write_file(invalid, source)!
			for mode in ['-autofree', '-ownership'] {
				for prealloc in [false, true] {
					mut command := [compiler, '-new-compiler', '-no-closures', '-gc', 'none', mode]
					if prealloc { command << '-prealloc' }
					command << ['-check', invalid]
					result := os.exec(command)
					assert result.exit_code != 0
					assert result.output.contains('borrowed by `alias`'), result.output
					assert result.output.contains('ambiguous.v:10:'), result.output
				}
			}
		}
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
	moved_source := 'struct Item implements Drop { code int }
fn (mut item Item) drop() {}
type Choice = int | &Item
struct Holder { mut: value Choice }
fn main() {
 mut source := Holder{value: Choice(&Item{code: 1})}
 mut destination := Holder{value: Choice(0)}
 pointer := source.value as &Item
 source.value = Choice(pointer)
 destination.value = source.value
 assert (destination.value as &Item).code == 1
 assert (source.value as &Item).code == 1
}'
	os.write_file(invalid, moved_source.replace(' assert (source.value as &Item).code == 1\n', ''))!
	control := os.exec([compiler, '-new-compiler', '-no-closures', '-gc', 'none', '-ownership',
		'run', invalid])
	assert control.exit_code == 0, control.output
	os.write_file(invalid, moved_source)!
	moved := os.exec([compiler, '-new-compiler', '-no-closures', '-gc', 'none', '-ownership', '-check',
		invalid])
	assert moved.exit_code != 0
	assert moved.output.contains('use of moved value: `source.value`'), moved.output
	assert moved.output.contains('ambiguous.v:12:'), moved.output
	for source in mixed_sources {
		os.write_file(invalid, source.replace('mut local :=', 'local :=').replace(' local = Item{code: 3}\n', '').replace(' assert alias is &Item', ' assert alias is &Item\n assert (value as &Item).code == 1'))!
		for prealloc in [false, true] {
			mut command := [compiler, '-new-compiler', '-no-closures', '-gc', 'none', '-ownership']
			if prealloc { command << '-prealloc' }
			command << ['-check', invalid]
			mixed_moved := os.exec(command)
			assert mixed_moved.exit_code != 0
			assert mixed_moved.output.contains('use of moved value: `value`'), mixed_moved.output
			assert mixed_moved.output.contains('ambiguous.v:13:'), mixed_moved.output
		}
	}
	storage_move := 'struct Item implements Drop { code int }
fn (mut item Item) drop() {}
type Choice = int | &Item
struct Holder { value Choice }
fn maybe(owned bool, item &Item) Choice {
 if owned { return Choice(&Item{code: 2}) }
 return Choice(item)
}
fn main() {
 local := Item{code: 1}
 source := SOURCE_INIT
 alias := Choice(SOURCE_SLOT)
 assert (alias as &Item).code == 2
 assert (SOURCE_SLOT as &Item).code == 2
}'
	for slot, initializer in {
		'source.value': 'Holder{value: Choice(maybe(true, &local))}'
		'source[0]':    '[Choice(maybe(true, &local))]'
	} {
		source := storage_move.replace('SOURCE_INIT', initializer).replace('SOURCE_SLOT', slot)
		for prealloc in [false, true] {
			mut command := [compiler, '-new-compiler', '-no-closures', '-gc', 'none', '-ownership']
			if prealloc { command << '-prealloc' }
			os.write_file(invalid, source.replace(' assert (${slot} as &Item).code == 2\n', ''))!
			mut control_command := command.clone()
			control_command << ['run', invalid]
			storage_control := os.exec(control_command)
			assert storage_control.exit_code == 0, storage_control.output
			os.write_file(invalid, source)!
			command << ['-check', invalid]
			storage_moved := os.exec(command)
			assert storage_moved.exit_code != 0
			assert storage_moved.output.contains('use of moved value: `${slot}`'), storage_moved.output
			assert storage_moved.output.contains('ambiguous.v:14:'), storage_moved.output
		}
	}
}
