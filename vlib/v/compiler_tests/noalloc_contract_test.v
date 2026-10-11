import os

struct NoallocCase {
	name     string
	accepted bool
	source   string
}

fn test_noalloc_contracts_in_build_and_check() {
	root := os.join_path(os.vtmp_dir(), 'noalloc_contracts_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	cases := [
		NoallocCase{
			name:     'recursive'
			accepted: true
			source:   '@[noalloc]
fn factorial(n int) int { if n < 2 { return 1 }; return n * factorial(n - 1) }
fn main() { _ = factorial(5); _ = [1, 2, 3] }
'
		}
		NoallocCase{
			name:     'fixed_scalar'
			accepted: true
			source:   '@[noalloc]
fn scalar(n int) int { values := [1, 2, 3]!; return values[n % 3] }
fn main() { _ = scalar(0) }
'
		}
		NoallocCase{
			name:     'buffer_growth'
			accepted: true
			source:   '@[noalloc]
fn append(mut out []u8, value u8) { out << value }
fn main() { mut out := []u8{cap: 32}; append(mut out, 1) }
'
		}
		NoallocCase{
			name:     'strict_buffer'
			accepted: false
			source:   '@[noalloc: strict]
fn append(mut out []u8, value u8) { out << value }
fn main() { mut out := []u8{}; append(mut out, 1) }
'
		}
		NoallocCase{
			name:     'buffer_slice'
			accepted: false
			source:   '@[noalloc]
fn append(mut out []u8) { digits := [u8(1), 2, 3]!; out << digits[..] }
fn main() { mut out := []u8{}; append(mut out) }
'
		}
		NoallocCase{
			name:     'array_loop'
			accepted: false
			source:   '@[noalloc]
fn sum(a int, b int) int { mut n := 0; for value in [a, b, a + b] { n += value }; return n }
fn main() { _ = sum(1, 2) }
'
		}
		NoallocCase{
			name:     'fixed_storage_escape'
			accepted: false
			source:   'struct Slice { start int; len int }
struct Params { mut: n int; vals [8]Slice }
fn update(mut p Params) { p.n++ }
@[noalloc]
fn handler() int { mut p := Params{}; update(mut p); return p.n }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'error_box'
			accepted: false
			source:   "fn parse(s string) !int { if s.len == 0 { return error('empty') }; return s.len }
@[noalloc]
fn handler() int { return parse('') or { 0 } }
fn main() { _ = handler() }
"
		}
		NoallocCase{
			name:     'bound_method'
			accepted: false
			source:   'struct App { base int }
fn (app &App) one(x int) int { return app.base + x }
@[noalloc]
fn handler(app &App) int { method := unsafe { app.one }; return method(1) }
fn main() { _ = handler(&App{base: 1}) }
'
		}
		NoallocCase{
			name:     'substring'
			accepted: false
			source:   "@[noalloc]
fn handler(text string) int { return text.all_after(' ').len }
fn main() { _ = handler('GET /users') }
"
		}
		NoallocCase{
			name:     'int_to_string'
			accepted: false
			source:   'fn middle(n int) int { return n.str().len }
@[noalloc]
fn handler(n int) int { return middle(n) }
fn main() { _ = handler(3) }
'
		}
		NoallocCase{
			name:     'own_promise'
			accepted: false
			source:   '@[noalloc]
fn leaf(n int) string { return n.str() }
@[noalloc]
fn handler() int { return leaf(3).len }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'sum_return'
			accepted: false
			source:   'struct Item { n int }
type Value = Item | int
@[noalloc]
fn handler() Value { return Item{n: 1} }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'callback_alias'
			accepted: false
			source:   'fn callback() int { return 1 }
fn invoke(f fn () int) int { callback := f; return callback() }
fn allocating() int { return [1, 2].len }
@[noalloc]
fn handler() int { return invoke(allocating) }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'branch_before_panic'
			accepted: false
			source:   "@[noalloc]
fn handler(flag bool, text string) string { if flag { return text + text }; panic('stopped') }
fn main() { _ = handler(true, 'value') }
"
		}
		NoallocCase{
			name:     'generic_allocation'
			accepted: false
			source:   'fn stringify[T](n T) string { return n.str() }
@[noalloc]
fn handler() int { return stringify[int](3).len }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'unsupported_alias'
			accepted: false
			source:   '@[noalloc]
type Callback = fn () int
fn main() { _ = Callback(fn () int { return 1 }) }
'
		}
		NoallocCase{
			name:     'invalid_mode'
			accepted: false
			source:   '@[noalloc: invalid]
fn handler() int { return 1 }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'foreign_contract'
			accepted: true
			source:   '@[noalloc]
fn C.pure_external(n int) int
@[noalloc]
fn handler() int { return C.pure_external(1) }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'foreign_unknown'
			accepted: false
			source:   'fn C.external(n int) int
@[noalloc]
fn handler() int { return C.external(1) }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'default_call'
			accepted: false
			source:   'fn allocating() int { return [1, 2].len }
struct Item { n int = allocating() }
@[noalloc]
fn handler() int { return Item{}.n }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'terminal'
			accepted: true
			source:   '@[noalloc]
fn handler() { panic([1, 2].str()) }
fn main() { if false { handler() } }
'
		}
		NoallocCase{
			name:     'generic_root_pure'
			accepted: true
			source:   '@[noalloc]
fn identity[T](value T) T { return value }
fn main() { _ = identity[int](2) }
'
		}
		NoallocCase{
			name:     'generic_root_allocating'
			accepted: false
			source:   '@[noalloc]
fn stringify[T](value T) string { return value.str() }
fn main() { _ = stringify[int](2) }
'
		}
		NoallocCase{
			name:     'interface_contract'
			accepted: false
			source:   '@[noalloc]
interface Shape { area() int }
fn main() {}
'
		}
		NoallocCase{
			name:     'foreign_only_invalid'
			accepted: false
			source:   '@[noalloc: invalid]
fn C.external(n int) int
fn main() {}
'
		}
		NoallocCase{
			name:     'interface_field_assignment'
			accepted: false
			source:   'interface Shape { area() int }
struct Square { side int }
fn (s Square) area() int { return s.side * s.side }
struct Holder { mut: item Shape }
@[noalloc]
fn handler(mut h Holder) { h.item = Square{side: 2} }
fn main() { mut h := Holder{item: Square{side: 1}}; handler(mut h) }
'
		}
		NoallocCase{
			name:     'interface_buffer_append'
			accepted: false
			source:   'interface Shape { area() int }
struct Square { side int }
fn (s Square) area() int { return s.side * s.side }
@[noalloc]
fn handler(mut out []Shape) { out << Square{side: 2} }
fn main() { mut out := []Shape{}; handler(mut out) }
'
		}
		NoallocCase{
			name:     'callback_parameter_shadow'
			accepted: false
			source:   'fn callback() int { return 1 }
fn invoke(callback fn () int) int { return callback() }
fn allocating() int { return [1, 2].len }
@[noalloc]
fn handler() int { return invoke(allocating) }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'callback_named_panic'
			accepted: false
			source:   'fn invoke(panic fn () int) int { return panic() }
fn allocating() int { return [1, 2].len }
@[noalloc]
fn handler() int { return invoke(allocating) }
fn main() { _ = handler() }
'
		}
		NoallocCase{
			name:     'fixed_default'
			accepted: false
			source:   'fn allocating() int { return [1, 2].len }\nstruct Inner { n int = allocating() }\n@[noalloc]\nfn handler() int { values := [2]Inner{}; return values[0].n }\nfn main() { _ = handler() }\n'
		}
		NoallocCase{
			name:     'nested_default'
			accepted: false
			source:   'fn allocating() int { return [1, 2].len }
struct Inner { n int = allocating() }
struct Outer { inner Inner }
@[noalloc]
fn handler() int { return Outer{}.inner.n }
fn main() { _ = handler() }
'
		},
		NoallocCase{
			name:     'scalar_struct'
			accepted: true
			source:   'struct Point { n int }
@[noalloc]
fn handler(n int) int { p := Point{n: n}; return p.n }
fn main() { _ = handler(1) }
'
		}
		NoallocCase{
			name:     'empty_function'
			accepted: true
			source:   '@[noalloc]
fn handler() {}
fn main() { handler() }
'
		}
		NoallocCase{
			name:     'read_byte_helper'
			accepted: true
			source:   'fn read(req []u8) u8 { return req[0] }
@[noalloc]
fn handler(req []u8) u8 { return read(req) }
fn main() { _ = handler([u8(1)]) }
'
		},
		NoallocCase{
			name:     'custom_operator'
			accepted: false
			source:   'struct Value { n int }
fn (a Value) + (b Value) Value {
    _ = [a.n, b.n].str()
    return Value{ n: a.n + b.n }
}
@[noalloc]
fn handler() int { return (Value{ n: 1 } + Value{ n: 2 }).n }
fn main() { _ = handler() }
'
		},
		NoallocCase{
			name:     'dereferenced_sum_assignment'
			accepted: false
			source:   'struct Item { n int }
type Value = Item | int
@[noalloc]
fn handler(out &Value) { unsafe { *out = Item{n: 1} } }
fn main() { mut out := Value(1); handler(&out) }
'
		},
		NoallocCase{
			name:     'growth_name_shadow'
			accepted: false
			source:   'fn array_push(mut out []u8, value u8) { _ = [value].len; out << value }
@[noalloc]
fn handler(mut out []u8, value u8) { array_push(mut out, value) }
fn main() { mut out := []u8{}; handler(mut out, 1) }
'
		},
		NoallocCase{
			name:     'bulk_self_append'
			accepted: false
			source:   '@[noalloc]\nfn handler(mut out []u8) { out << out }\nfn main() { mut out := [u8(1)]; handler(mut out) }\n'
		},
		NoallocCase{
			name:     'transitive_generic_pure'
			accepted: true
			source:   'fn identity[T](value T) T { return value }\n@[noalloc]\nfn handler(value int) int { return identity[int](value) }\nfn main() { _ = handler(1) }\n'
		},
		NoallocCase{
			name:     'range_loop_scalar_body'
			accepted: true
			source:   'struct Point { n int }\n@[noalloc]\nfn handler(n int) int { mut sum := 0; for i in 0 .. n { p := Point{n: i}; sum += p.n }; return sum }\nfn main() { _ = handler(2) }\n'
		},
		NoallocCase{
			name:     'callstyle_extra_argument'
			accepted: false
			source:   "@[noalloc('strict', 'unexpected')]\nfn handler() int { return 1 }\nfn main() { _ = handler() }\n"
		},
		NoallocCase{
			name:     'callstyle_named_argument'
			accepted: false
			source:   "@[noalloc(mode: 'strict')]\nfn handler() int { return 1 }\nfn main() { _ = handler() }\n"
		},
		NoallocCase{
			name:     'foreign_extra_argument'
			accepted: false
			source:   "@[noalloc('strict', 'unexpected')]\nfn C.external(n int) int\nfn main() {}\n"
		},
		NoallocCase{
			name:     'callstyle_strict_buffer'
			accepted: false
			source:   "@[noalloc('strict')]\nfn handler(mut out []u8) { out << u8(1) }\nfn main() { mut out := []u8{}; handler(mut out) }\n"
		},
		NoallocCase{
			name:     'alias_custom_operator'
			accepted: false
			source:   'type Value = int\nfn (a Value) + (b Value) Value { _ = [1, 2].len; return Value(int(a) + int(b)) }\n@[noalloc]\nfn handler(a Value, b Value) Value { return a + b }\nfn main() { _ = handler(Value(1), Value(2)) }\n'
		},
		NoallocCase{
			name:     'alias_compound_operator'
			accepted: false
			source:   'type Value = int\nfn (a Value) + (b Value) Value { _ = [1, 2].len; return Value(int(a) + int(b)) }\n@[noalloc]\nfn handler(a Value, b Value) Value { mut n := a; n += b; return n }\nfn main() { _ = handler(Value(1), Value(2)) }\n'
		},
		NoallocCase{
			name:     'allocation_before_terminal'
			accepted: false
			source:   '@[noalloc]\nfn handler() { values := [1, 2]; panic(values.str()) }\nfn main() { if false { handler() } }\n'
		},
		NoallocCase{
			name:     'generic_allocation_before_terminal'
			accepted: false
			source:   '@[noalloc]\nfn handler[T](value T) { values := [value]; panic(values.str()) }\nfn main() { if false { handler[int](1) } }\n'
		},
		NoallocCase{
			name:     'plain_numeric_alias'
			accepted: true
			source:   'type Value = int\n@[noalloc]\nfn handler(a Value, b Value) Value { return a + b }\nfn main() { _ = handler(Value(1), Value(2)) }\n'
		},
		NoallocCase{
			name:     'custom_scalar_error'
			accepted: false
			source:   "struct SmallError { n int }\nfn (e SmallError) msg() string { return 'small' }\nfn (e SmallError) code() int { return e.n }\n@[noalloc]\nfn handler() !int { return SmallError{n: 7} }\nfn main() { _ = handler() or { 0 } }\n"
		},
		NoallocCase{
			name:     'successful_scalar_result'
			accepted: true
			source:   '@[noalloc]\nfn handler(n int) !int { return n }\nfn main() { _ = handler(1) or { 0 } }\n'
		},
		NoallocCase{
			name:     'successful_error_struct_payload'
			accepted: true
			source:   "struct SmallError { n int }\nfn (e SmallError) msg() string { return 'small' }\nfn (e SmallError) code() int { return e.n }\n@[noalloc]\nfn handler() !SmallError { return SmallError{n: 7} }\nfn main() { _ = handler() or { SmallError{n: 0} } }\n"
		},
		NoallocCase{
			name:     'successful_error_pointer_payload'
			accepted: true
			source:   "struct SmallError { n int }\nfn (e SmallError) msg() string { return 'small' }\nfn (e SmallError) code() int { return e.n }\n@[noalloc]\nfn handler(value &SmallError) !&SmallError { return value }\nfn main() { _ = handler(&SmallError{n: 7}) or { &SmallError{n: 0} } }\n"
		},
	]
	// The standalone FastC parser must reject contracts it cannot check.
	fastc_source := os.join_path(root, 'fastc.v')
	os.write_file(fastc_source, '@[noalloc]\nfn handler() int { return 1 }\nfn main() { _ = handler() }\n') or { panic(err) }
	fastc_result := os.exec([@VEXE, '-b', 'fastc', '-o', os.join_path(root, 'fastc.c'), fastc_source])
	if !fastc_result.output.contains('fastc support is not compiled') {
		assert fastc_result.exit_code != 0 && fastc_result.output.contains('noalloc'), fastc_result.output
	}

	for case in cases {
		source := os.join_path(root, '${case.name}.c.v')
		os.write_file(source, case.source) or { panic(err) }
		for mode in ['build', 'check'] {
			mut command := [@VEXE, '-gc', 'none']
			if mode == 'check' {
				command << '-check'
			} else {
				command << ['-o', os.join_path(root, '${case.name}.c')]
			}
			command << source
			result := os.exec(command)
			assert (result.exit_code == 0) == case.accepted, '${case.name} ${mode}: ${result.output}'
			if !case.accepted {
				assert result.output.contains('noalloc'), '${case.name} ${mode}: ${result.output}'
			}
			if case.name == 'int_to_string' {
				assert result.output.contains('handler -> middle'), result.output
			}
		}
	}
}

fn test_noalloc_cannot_reuse_an_allocating_cached_dependency() {
	root := os.join_path(os.vtmp_dir(), 'noalloc_cached_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'allocdep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'allocdep', 'allocdep.v'),
		'module allocdep\npub fn value() int { return [1, 2].len }\n') or { panic(err) }
	source := os.join_path(root, 'main.v')
	program := 'import allocdep\nfn handler() int { return allocdep.value() }\nfn main() { _ = handler() }\n'
	os.write_file(source, program) or { panic(err) }
	command := [@VEXE, '-gc', 'none', '-o', os.join_path(root, 'program'), source]
	warmup := os.exec(command)
	assert warmup.exit_code == 0, warmup.output
	os.write_file(source, program.replace('fn handler()', '@[noalloc]\nfn handler()')) or {
		panic(err)
	}
	for _ in 0 .. 2 {
		result := os.exec(command)
		assert result.exit_code != 0 && result.output.contains('noalloc'), result.output
	}
}

fn test_noalloc_rejects_opaque_codegen_instrumentation() {
	root := os.join_path(os.vtmp_dir(), 'noalloc_instrumentation_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[noalloc]\nfn handler() int { return 1 }\nfn main() { _ = handler() }\n') or { panic(err) }
	// Exercise both allocation modes and tracing through their public defines or flags.
	for flags in [['-d', 'autofree'], ['-d', 'ownership'], ['-trace-calls']] {
		mut command := [@VEXE, '-gc', 'none', '-o', os.join_path(root, 'out.c')]
		command << flags
		command << source
		result := os.exec(command)
		assert result.exit_code != 0 && result.output.contains('noalloc'), result.output
	}
}
