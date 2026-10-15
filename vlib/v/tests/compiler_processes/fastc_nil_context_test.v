module main

import os
import v.cmdexec

struct FastcNilCase {
	name   string
	source string
	c_file bool
}

fn test_fastc_nil_uses_concrete_expected_types() {
	$if !macos && !linux {
		return
	}

	cases := [
		FastcNilCase{'alias_field', r'module main
type Handle = voidptr
struct Holder { mut: h Handle }
fn main() { mut s := Holder{}; s.h = nil; assert isnil(s.h); println("ok") }
', false},
		FastcNilCase{'array', r'module main
fn main() { values := [voidptr(0), nil]; assert values.len == 2; assert isnil(values[1]); println("ok") }
', false},
		FastcNilCase{'c_typed', r'module main
struct Node { value int }
fn take(n &Node) bool { return isnil(n) }
fn main() { assert take(nil); println("ok") }
', true},
		FastcNilCase{'comparison', r'module main
fn main() { p := voidptr(0); assert p == nil; assert !(p != nil); println("ok") }
', false},
		FastcNilCase{'comparison_chain', r'module main
fn main() { p := voidptr(0); ready := true; assert p == nil && ready; assert p != nil || ready; println("ok") }
', false},
		FastcNilCase{'array_return', r'module main
fn pointers() []voidptr { return [voidptr(0), nil] }
fn main() { values := pointers(); assert values.len == 2; assert isnil(values[1]); println("ok") }
', false},
		FastcNilCase{'array_field', r'module main
struct Holder { values []voidptr }
fn main() { h := Holder{values: [voidptr(0), nil]}; assert isnil(h.values[1]); println("ok") }
', false},
		FastcNilCase{'static_method', r'module main
struct Tool {}
fn Tool.accept(p voidptr) bool { return isnil(p) }
fn main() { assert Tool.accept(nil); println("ok") }
', false},
		FastcNilCase{'default', r'module main
struct Holder { p voidptr = nil }
fn main() { holder := Holder{}; assert isnil(holder.p); println("ok") }
', false},
		FastcNilCase{'generic_collision', r'module main
import other
fn consume(p voidptr) bool { return isnil(p) }
fn main() { assert other.marker == 1; assert consume(nil); println("ok") }
', false},
		FastcNilCase{'local', r'module main
fn main() { mut vp := voidptr(0); vp = nil; assert isnil(vp); println("ok") }
', false},
		FastcNilCase{'method', r'module main
struct Item {}
fn (s Item) is_null(p voidptr) bool { return isnil(p) }
fn main() { assert Item{}.is_null(nil); println("ok") }
', false},
		FastcNilCase{'parenthesized', r'module main
fn is_null(p voidptr) bool { return isnil(p) }
fn main() { assert is_null((nil)); println("ok") }
', false},
		FastcNilCase{'return', r'module main
fn empty_pointer() voidptr { return nil }
fn main() { assert isnil(empty_pointer()); println("ok") }
', false},
		FastcNilCase{'variadic', r'module main
fn count(values ...voidptr) int { return values.len }
fn main() { assert count(nil, nil) == 2; println("ok") }
', false},
	]
	for case in cases {
		root := os.join_path(os.vtmp_dir(), 'fastc_nil_${case.name}_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		os.write_file(os.join_path(root, if case.c_file { 'main.c.v' } else { 'main.v' }), case.source)!
		if case.name == 'generic_collision' {
			os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'nil_collision' }")!
			os.mkdir_all(os.join_path(root, 'other'))!
			os.write_file(os.join_path(root, 'other', 'other.v'), 'module other\npub const marker = 1\npub fn consume[T](value T) T { return value }\n')!
		}
		for backend in ['c', 'fastc'] {
			executable := os.join_path(root, 'program_${backend}')
			build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-gc', 'none', '-o', executable,
				root])
			assert build.exit_code == 0, '${case.name} ${backend}: ${build.output}'
			run := cmdexec.run(executable, [])
			assert run.exit_code == 0, '${case.name} ${backend}: ${run.output}'
			assert run.output.trim_space() == 'ok', '${case.name} ${backend}: ${run.output}'
		}
	}
}

fn test_fastc_nil_still_rejects_erased_function_and_typed_pointer_storage() {
	$if !macos && !linux {
		return
	}

	sources := [
		'module main\nfn main() { mut callback := fn () {}; callback = nil }\n',
		'module main\nstruct Holder { mut: callback fn () = unsafe { nil } }\nfn main() { mut h := Holder{}; h.callback = nil }\n',
		'module main\ntype Callback = fn ()\nfn main() { mut callback := Callback(fn () {}); callback = nil }\n',
		'module main\nfn take(p &int) {}\nfn main() { take((nil)) }\n',
		'module main\nstruct Holder { p voidptr }\nfn take(p &int) voidptr { return voidptr(p) }\nfn main() { _ = Holder{p: take(nil)} }\n',
	]
	for i, source in sources {
		root := os.join_path(os.vtmp_dir(), 'fastc_nil_negative_${i}_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		os.write_file(os.join_path(root, 'main.v'), source)!
		for backend in ['c', 'fastc'] {
			result := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-gc', 'none', '-o',
				os.join_path(root, 'program_${backend}'), root])
			assert result.exit_code != 0, '${i} ${backend} unexpectedly accepted nil'
		}
	}
}
