module fastc

import os
import v.cmdexec

fn test_fastc_runtime_values_across_closures_options_collections_and_native_compilers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_runtime_values_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

type Callback = fn (int)
type Hook = fn (int) !
type Values = map[string]int
type Value = Scalar | Nested
struct Scalar { value ?int }
struct Nested { values []Value mapping map[string]int }
struct Item { value int }
interface Reading { read() int }
struct Payload { value int }
fn (payload Payload) read() int { return payload.value }
struct Holder { item Reading }
struct Loader { fetch fn (int) int = send }
enum Slot { first second count }
struct Buckets { values [int(Slot.count)][]Item }
type ChannelCallback = fn (chan int)
fn deliver(channel chan int) { channel <- 42 }
fn send(value int) int { return value + 1 }

fn (item &Item) check() { assert item.value == 42 }
fn notify(value int) { assert value == 42 }
fn hook(value int) ! { assert value == 42 }
fn counter() fn () int {
 mut count := 0
 return fn [mut count] () int { count++; return count }
}
fn wrap(optional ?Hook) fn () {
 return fn [optional] () { if call := optional { call(42) or { panic(err) } } }
}
fn maybe_item() ?&Item { return &Item{42} }
fn main() {
 assert `\u00a0` == rune(160)
 assert `\U0001f600` == rune(0x1f600)
 mut next := counter()
 assert next() == 1
 assert next() == 2
 wrapped := wrap(hook)
 wrapped()
 mut names := Values{}
 mut add := fn [mut names] (name string) { names[name] = 42 }
 nested := fn [mut add] () { add("answer") }
 nested()
 callbacks := [Callback(notify)]
 for callback in callbacks { callback(42) }
 callbacks2 := []fn (int){len: 1, init: notify}
 for callback in callbacks2 { callback(42) }
 mut options := []?string{}
 options << ?string(none)
 options << "hello"
 assert options[0] == none
 assert options[1] or { "" } == "hello"
 if value := options[1] { assert value == "hello" }
 a := Nested{values: [Value(Scalar{value: 42})], mapping: {"a": 1, "b": 2}}
 b := Nested{values: [Value(Scalar{value: 42})], mapping: {"b": 2, "a": 1}}
 c := Nested{values: [Value(Scalar{value: 43})], mapping: {"a": 1, "b": 2}}
 assert Holder{item: Payload{42}} == Holder{item: Payload{42}}
 assert Holder{item: Payload{42}} != Holder{item: Payload{43}}
 assert a == b
 assert a != c
 for item := ?&Item(&Item{42}); item != none; {
  current := item or { break }
  current.check()
  break
 }
 item := &Item{42}
 unsafe { &Item(item) }
 .check()
 assert item.value
  // A comment between the operand and its continuation.
  != 0
 values := [1, 2]
 mut sum := 0
 for _ in 0 .. 1 {
  for value in [options[0] or { continue }, options[1] or { continue }] { sum += value.len }
 }
 assert sum == 0
 _ = values
 mut buckets := Buckets{}
 buckets.values[0] << Item{42}
 assert buckets.values[0][0].value == 42
 channel := chan int{cap: 1}
 callback := ChannelCallback(deliver)
 callback(channel)
 received := <-channel
 assert received == 42
 loader := Loader{fetch: send}
 worker := spawn loader.fetch(41)
 assert worker.wait() == 42

 value := if outer := maybe_item() {
  if inner := maybe_item() { inner.value } else { outer.value }
 } else { 0 }
 assert value == 42
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
