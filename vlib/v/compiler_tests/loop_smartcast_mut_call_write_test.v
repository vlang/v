import os
import v.tempname

fn loop_smartcast_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_loop_smartcast_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

struct LoopSmartcastCase {
	name     string
	source   string
	expected string
}

fn loop_smartcast_compile_batch(bin string, cases []LoopSmartcastCase) os.Result {
	root := os.dir(bin)
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		module_dir := os.join_path(root, module_name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		fixture := 'module ${module_name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, 'v3_${case.name}.v'), fixture.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	os.write_file(os.join_path(root, 'main.v'), "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	return os.exec([@VEXE, '-b', 'c', '-nocolor', '-o', bin, root])
}

fn loop_smartcast_run_good_batch(cases []LoopSmartcastCase) {
	root := loop_smartcast_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	bin := os.join_path(root, 'program')
	compile := loop_smartcast_compile_batch(bin, cases)
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	for case in cases {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn loop_smartcast_run_bad_batch(cases []LoopSmartcastCase) {
	root := loop_smartcast_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	compile := loop_smartcast_compile_batch(os.join_path(root, 'program'), cases)
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	mut errors := map[string][]string{}
	for line in compile.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		// Drop the line and column from the right to preserve any colon in the path.
		file := location.all_before_last(':').all_before_last(':')
		errors[os.file_name(file)] << message
	}
	for case in cases {
		assert errors['v3_${case.name}.v'].any(it.contains(case.expected)), '${case.name}: expected `${case.expected}` in\n${compile.output}'
	}
}

fn test_loop_mutation_invalidates_smartcasts() {
	loop_smartcast_run_bad_batch([
		// A loop body that passes the loop-narrowed sum value to a `mut` sum parameter lets
		// the callee swap the active variant. The loop-condition narrowing must not be
		// restored for a later variant access, otherwise the checker accepts a field that no
		// longer exists at runtime and emits unsafe C (reading the wrong variant's storage).
		LoopSmartcastCase{
			name:     'bad_loop_mut_sum_call_stale_smartcast'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn replace(mut v Value) {
	v = Value(Bar{})
}

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	for x is Foo {
		replace(mut x)
		println(int_str(x.foo))
		break
	}
}
'
			expected: 'field `foo` does not exist'
		},
		// A mut-receiver method on the declared sum type resolves through that sum and can
		// swap the active variant (`x.replace()` where `fn (mut v Value) replace()`). The
		// receiver is stored under the call's selector child rather than among its `mut`
		// arguments, so it must be recognised as a write too, otherwise the loop narrowing
		// is wrongly restored for a later variant access and unsafe C is emitted.
		LoopSmartcastCase{
			name:     'bad_loop_mut_sum_receiver_stale_smartcast'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn (mut v Value) replace() {
	v = Value(Bar{})
}

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	for x is Foo {
		x.replace()
		println(int_str(x.foo))
		break
	}
}
'
			expected: 'field `foo` does not exist'
		},
		// The loop narrows a nested field `holder.value`, and a mut-receiver method resolves
		// through its declared sum type and can swap the active variant. The receiver is the
		// full field-access key `holder.value`, not a bare identifier, so it must still be
		// recognised as a write; otherwise the loop narrowing is restored for a later
		// `holder.value.foo` and unsafe C is emitted.
		LoopSmartcastCase{
			name:     'bad_loop_nested_mut_sum_receiver'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn (mut v Value) replace() {
	v = Value(Bar{})
}

fn main() {
	mut h := Holder{
		value: Value(Foo{
			foo: 1
		})
	}
	for h.value is Foo {
		h.value.replace()
		println(int_str(h.value.foo))
		break
	}
}
'
			expected: 'field `foo` does not exist'
		},
		// The loop narrows `holder.value`, and reassigning the parent `holder` replaces the
		// storage that field reads, so its runtime tag no longer holds. A write to the
		// ancestor key must invalidate the narrowed descendant; otherwise the loop narrowing
		// is restored for a later `holder.value.foo` and unsafe C is emitted.
		LoopSmartcastCase{
			name:     'bad_loop_parent_assign_stale_nested'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn main() {
	mut holder := Holder{
		value: Value(Foo{
			foo: 1
		})
	}
	for holder.value is Foo {
		holder = Holder{
			value: Value(Bar{})
		}
		println(int_str(holder.value.foo))
		break
	}
}
'
			expected: 'field `foo` does not exist'
		},
		// The narrowed receiver contains an index (`items[0].value`). The mut-receiver sum
		// method resolves through the declared element type and can swap the active variant,
		// so it must be recognised as a write even though the receiver is not a plain
		// identifier/selector chain.
		LoopSmartcastCase{
			name:     'bad_loop_indexed_mut_sum_receiver'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn (mut v Value) replace() {
	v = Value(Bar{})
}

fn main() {
	mut items := [Holder{
		value: Value(Foo{
			foo: 1
		})
	}]
	for items[0].value is Foo {
		items[0].value.replace()
		println(int_str(items[0].value.foo))
		break
	}
}
'
			expected: 'field `foo` does not exist'
		},
		// Passing the parent `holder` to a `mut` parameter lets the callee reassign the
		// narrowed field `holder.value`, so it must count as a write to `holder.value`
		// even though the argument key is only an ancestor of the narrowed key.
		LoopSmartcastCase{
			name:     'bad_loop_mut_ancestor_arg'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn replace(mut h Holder) {
	h.value = Value(Bar{})
}

fn main() {
	mut holder := Holder{
		value: Value(Foo{
			foo: 1
		})
	}
	for holder.value is Foo {
		replace(mut holder)
		println(int_str(holder.value.foo))
		break
	}
}
'
			expected: 'field `foo` does not exist'
		},
		// A mut-receiver method on the ancestor `holder` can likewise reassign the narrowed
		// field, so the ancestor receiver counts as a write too.
		LoopSmartcastCase{
			name:     'bad_loop_mut_ancestor_receiver'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn (mut h Holder) reset() {
	h.value = Value(Bar{})
}

fn main() {
	mut holder := Holder{
		value: Value(Foo{
			foo: 1
		})
	}
	for holder.value is Foo {
		holder.reset()
		println(int_str(holder.value.foo))
		break
	}
}
'
			expected: 'field `foo` does not exist'
		},
	])
}

fn test_loop_smartcast_runtime_cases() {
	loop_smartcast_run_good_batch([
		// Without a body write the loop-condition narrowing stays valid.
		LoopSmartcastCase{
			name:     'good_loop_smartcast_without_write'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn main() {
	mut x := Value(Foo{
		foo: 7
	})
	for x is Foo {
		println(int_str(x.foo))
		break
	}
}
'
			expected: '7'
		},
		// A `mut Variant` parameter cannot change the runtime tag, so passing the narrowed
		// value to it leaves the narrowing intact and the mutation is observed (no false
		// positive: the smartcast survives the call).
		LoopSmartcastCase{
			name:     'good_mut_variant_call_keeps_smartcast'
			source:   'struct Foo {
mut:
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn bump(mut f Foo) {
	f.foo = 9
}

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	for x is Foo {
		bump(mut x)
		println(int_str(x.foo))
		break
	}
}
'
			expected: '9'
		},
		// A mut-receiver method on the narrowed variant type cannot change the runtime
		// tag, so the narrowing survives and the mutation is observed (no false positive).
		LoopSmartcastCase{
			name:     'good_loop_mut_variant_receiver_keeps_smartcast'
			source:   'struct Foo {
mut:
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn (mut f Foo) bump() {
	f.foo = 9
}

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	for x is Foo {
		x.bump()
		println(int_str(x.foo))
		break
	}
}
'
			expected: '9'
		},
		// A read-only method on the sum receiver is not a write, so the narrowing stays.
		LoopSmartcastCase{
			name:     'good_loop_readonly_receiver_keeps_smartcast'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn (v Value) describe() int {
	return 1
}

fn main() {
	mut x := Value(Foo{
		foo: 5
	})
	for x is Foo {
		_ := x.describe()
		println(int_str(x.foo))
		break
	}
}
'
			expected: '5'
		},
		// A mut-receiver method on the narrowed variant type keeps the nested narrowing.
		LoopSmartcastCase{
			name:     'good_loop_nested_mut_variant_receiver'
			source:   'struct Foo {
mut:
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn (mut f Foo) bump() {
	f.foo = 9
}

fn main() {
	mut h := Holder{
		value: Value(Foo{
			foo: 1
		})
	}
	for h.value is Foo {
		h.value.bump()
		println(int_str(h.value.foo))
		break
	}
}
'
			expected: '9'
		},
		// Writing a descendant field (`holder.value.foo`) does not change the tag, so the
		// nested narrowing survives.
		LoopSmartcastCase{
			name:     'good_loop_descendant_write_keeps_nested'
			source:   'struct Foo {
mut:
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn main() {
	mut holder := Holder{
		value: Value(Foo{
			foo: 1
		})
	}
	for holder.value is Foo {
		holder.value.foo = 2
		println(int_str(holder.value.foo))
		break
	}
}
'
			expected: '2'
		},
		// A mut-receiver method on the narrowed variant type keeps the indexed narrowing.
		LoopSmartcastCase{
			name:     'good_loop_indexed_mut_variant_receiver'
			source:   'struct Foo {
mut:
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn (mut f Foo) bump() {
	f.foo = 9
}

fn main() {
	mut items := [Holder{
		value: Value(Foo{
			foo: 1
		})
	}]
	for items[0].value is Foo {
		items[0].value.bump()
		println(int_str(items[0].value.foo))
		break
	}
}
'
			expected: '9'
		},
		// A read-only method on the ancestor is not a write, so the narrowing stays.
		LoopSmartcastCase{
			name:     'good_loop_readonly_ancestor_receiver'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn (h Holder) describe() int {
	return 1
}

fn main() {
	mut holder := Holder{
		value: Value(Foo{
			foo: 7
		})
	}
	for holder.value is Foo {
		_ := holder.describe()
		println(int_str(holder.value.foo))
		break
	}
}
'
			expected: '7'
		},
		// A `mut` argument that is neither the key nor an ancestor of it keeps the narrowing.
		LoopSmartcastCase{
			name:     'good_loop_unrelated_mut_arg'
			source:   'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value Value
}

fn touch(mut xs []int) {
	xs << 1
}

fn main() {
	mut holder := Holder{
		value: Value(Foo{
			foo: 4
		})
	}
	mut other := []int{}
	for holder.value is Foo {
		touch(mut other)
		println(int_str(holder.value.foo))
		break
	}
}
'
			expected: '4'
		},
		LoopSmartcastCase{
			name:     'nearest_mixed_branch_smartcasts'
			source:   'struct A { value int }
struct B {}
struct C {}
type Inner = A | B
type Outer = C | Inner
fn from_if(v Outer) int {
 if v is Inner {
  match v {
   A { return v.value }
   else {}
  }
 }
 return 0
}
fn from_match(v Outer) int {
 match v {
  Inner {
   if v is A { return v.value }
  }
  else {}
 }
 return 0
}
fn from_loop(v Outer) int {
 if v is Inner {
  for v is A { return v.value }
 }
 return 0
}
fn main() {
 a := Outer(Inner(A{value: 42}))
 println(from_if(a))
 println(from_match(a))
 println(from_loop(a))
}
'
			expected: '42\n42\n42'
		},
	])
}
